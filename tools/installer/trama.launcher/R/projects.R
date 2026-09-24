#' Projetos do trama: pastas abertas com `trama::tr_project()`. A tela de
#' início lista a pasta padrão de projetos mais os recentes guardados no
#' estado, oferece criar um projeto novo e abrir um projeto num processo R
#' separado, cada um na sua porta.
#' @noRd
NULL

#' Pasta "Documentos" real do usuário no Windows: lida do registro
#' (`User Shell Folders`), porque o OneDrive costuma redirecionar
#' `Documents`/`Documentos` para dentro de `%USERPROFILE%\OneDrive\...` — um
#' `file.path(USERPROFILE, "Documents")` fixo erraria a pasta nesse caso
#' (criaria uma "Documents" nova, fora do OneDrive, em vez de usar a de
#' verdade). O valor no registro pode vir como `%USERPROFILE%\...` (variável
#' de ambiente não expandida) — daí o `gsub` que expande manualmente.
#'
#' `ler_registro` existe como parâmetro (mesmo padrão de `.tl_janela_comando()`
#' em R/window.R) para os testes poderem mockar sem `utils::readRegistry()`
#' de verdade, que só funciona no Windows.
#' @noRd
.tl_documentos_windows <- function(ler_registro = .tl_ler_registro_documentos) {
  valor <- tryCatch(ler_registro(), error = function(e) NA_character_)
  if (is.null(valor) || is.na(valor) || !nzchar(valor)) {
    return(file.path(Sys.getenv("USERPROFILE"), "Documents"))
  }

  valor <- .tl_expandir_env_windows(valor)
  if (!nzchar(valor)) file.path(Sys.getenv("USERPROFILE"), "Documents") else valor
}

#' Expande ocorrências de `%VAR%` num caminho do Windows usando
#' `Sys.getenv()` — o registro guarda `Personal` (a pasta "Documentos") às
#' vezes como caminho literal, às vezes como `%USERPROFILE%\Documents` (não
#' expandido). Função à parte só para ficar testável sem regex inline.
#' @noRd
.tl_expandir_env_windows <- function(caminho) {
  achados <- regmatches(caminho, gregexpr("%[^%]+%", caminho))[[1]]
  for (v in achados) {
    caminho <- sub(v, Sys.getenv(gsub("%", "", v), unset = v), caminho, fixed = TRUE)
  }
  caminho
}

#' Lê `Personal` (a chave do "Documentos") de `User Shell Folders` no
#' registro do usuário atual (HCU). Só existe no Windows.
#' @noRd
.tl_ler_registro_documentos <- function() {
  chave <- "Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders"
  utils::readRegistry(chave, "HCU")$Personal
}

#' Pasta padrão onde os projetos do pesquisador ficam, sobrescrevível por
#' `TRAMA_PROJECTS` (testes e uso avançado).
#' @noRd
tl_projects_dir <- function() {
  p <- Sys.getenv("TRAMA_PROJECTS", "")
  if (nzchar(p)) return(normalizePath(p, mustWork = FALSE))

  if (.Platform$OS.type == "windows") {
    return(file.path(.tl_documentos_windows(), "Trama"))
  }
  documentos <- file.path(path.expand("~"), "Documentos")
  if (dir.exists(documentos)) file.path(documentos, "Trama")
  else file.path(path.expand("~"), "Documents", "Trama")
}

#' Nomes proibidos de projeto: os mesmos caracteres que o Windows já recusa
#' em nome de pasta, para um nome válido também valer no Linux e não travar
#' quando o projeto for aberto lá.
#' @noRd
.tl_projeto_nome_invalido <- function(nome) {
  !length(nome) || !nzchar(nome) || grepl('[/\\\\:*?"<>|]', nome)
}

#' Primeira porta da faixa usada pelos processos de projeto (o editor),
#' longe da porta do launcher (8725) e com folga suficiente para não colidir
#' com ela nem com a faixa que `.tl_porta_livre()` varre a partir daqui.
#' @noRd
.tl_porta_projetos_inicial <- function() 8740L

#' Registro em memória `caminho -> porta` dos projetos abertos nesta sessão
#' do launcher. Não é estado persistido: reiniciar o launcher esquece quais
#' portas estavam de pé (os processos do editor continuam vivos, só não são
#' mais rastreados aqui — a próxima abertura desse projeto sobe noutra porta).
#' @noRd
.tl_processos_projeto <- new.env(parent = emptyenv())

#' Porta em uso pelo editor de `caminho`, ou `NA` se este processo do
#' launcher nunca abriu esse projeto.
#' @noRd
tl_project_port <- function(caminho) {
  chave <- normalizePath(caminho, mustWork = FALSE)
  if (!exists(chave, envir = .tl_processos_projeto, inherits = FALSE)) return(NA_integer_)
  get(chave, envir = .tl_processos_projeto)
}

#' Um projeto está "aberto" quando a porta registrada para ele responde a
#' uma conexão — testa o processo de verdade, não só o registro em memória
#' (que não sabe se o `Rscript` do editor morreu).
#' @noRd
tl_project_aberto <- function(caminho) {
  porta <- tl_project_port(caminho)
  if (is.na(porta)) return(FALSE)
  con <- tryCatch(
    suppressWarnings(socketConnection(host = "127.0.0.1", port = porta, timeout = 1, open = "r")),
    error = function(e) NULL
  )
  if (is.null(con)) return(FALSE)
  close(con)
  TRUE
}

#' Lista os projetos: união das subpastas de `tl_projects_dir()` com os
#' `recentes` do estado (até 10, ignorando pastas que sumiram do disco).
#' @noRd
tl_projects <- function(s = tl_state_read()) {
  base <- tl_projects_dir()
  das_pasta <- if (dir.exists(base)) {
    list.dirs(base, recursive = FALSE, full.names = TRUE)
  } else character(0)

  recentes <- utils::head(s$recentes, 10)
  caminhos <- union(das_pasta, recentes)
  caminhos <- caminhos[dir.exists(caminhos)]

  if (!length(caminhos)) {
    return(data.frame(
      nome = character(0), caminho = character(0),
      modificado = as.POSIXct(character(0)), aberto = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(caminhos)
  data.frame(
    nome = basename(caminhos),
    caminho = caminhos,
    modificado = info$mtime,
    aberto = vapply(caminhos, tl_project_aberto, logical(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' Cria um projeto novo: valida o nome, cria a pasta dentro de
#' `tl_projects_dir()` e grava `trama.json` com as coleções pedidas
#' (núcleo `trama.data`/`trama.view` por padrão, mas desligável).
#'
#' `auto_unbox = FALSE` de propósito: uma coleção só tem que sair como array
#' `["trama.data"]` no JSON, nunca como a string nua `"trama.data"` — mesmo
#' motivo documentado em `R/project.R` (`tr_project_new()`, no pacote raiz).
#' @noRd
tl_project_new <- function(nome, colecoes = c("trama.data", "trama.view")) {
  if (.tl_projeto_nome_invalido(nome)) {
    stop(sprintf("'%s' não é um nome de projeto válido.", nome), call. = FALSE)
  }

  base <- tl_projects_dir()
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  caminho <- file.path(base, nome)
  if (dir.exists(caminho)) {
    stop(sprintf("Já existe um projeto chamado '%s'.", nome), call. = FALSE)
  }

  dir.create(caminho, recursive = TRUE)
  .tl_trama_json_escrever(caminho, list(collections = as.character(colecoes)))
  caminho
}

#' Caminho do manifesto de projeto (`trama.json`) dentro de `caminho`.
#' @noRd
.tl_trama_json <- function(caminho) file.path(caminho, "trama.json")

#' Lê `trama.json` de `caminho` como lista, ou lista vazia se o arquivo não
#' existir ainda (projeto criado fora do launcher, por exemplo).
#' @noRd
.tl_trama_json_ler <- function(caminho) {
  arquivo <- .tl_trama_json(caminho)
  if (!file.exists(arquivo)) return(list())
  jsonlite::fromJSON(arquivo, simplifyVector = FALSE)
}

#' Grava `cfg` (lista) em `trama.json` dentro de `caminho`, com
#' `collections` sempre como array (`auto_unbox = FALSE`) mesmo com um só
#' elemento — ver nota de `tl_project_new()`.
#' @noRd
.tl_trama_json_escrever <- function(caminho, cfg) {
  writeLines(
    jsonlite::toJSON(cfg, auto_unbox = FALSE, pretty = TRUE, null = "null"),
    .tl_trama_json(caminho)
  )
  invisible(caminho)
}

#' Coleções (nomes de pacote) que o `trama.json` de `caminho` pede. Vetor
#' vazio se o projeto não tiver manifesto ou não tiver `collections`.
#' @noRd
tl_project_collections <- function(caminho) {
  cfg <- .tl_trama_json_ler(caminho)
  if (is.null(cfg$collections)) character(0) else as.character(unlist(cfg$collections))
}

#' Grava `colecoes` no `trama.json` de `caminho`, preservando as demais
#' chaves do manifesto (ex.: `settings`/executor) — só `collections` é
#' sobrescrita.
#' @noRd
tl_project_set_collections <- function(caminho, colecoes) {
  cfg <- .tl_trama_json_ler(caminho)
  cfg$collections <- as.character(colecoes)
  .tl_trama_json_escrever(caminho, cfg)
  invisible(caminho)
}

#' Coleções que o projeto pede e que NÃO estão instaladas em `lib` (pasta
#' do pacote não existe ali). Usado ao abrir um projeto para oferecer
#' "Instalar e abrir" em vez de deixar `trama::tr_project()` recusar lá na
#' frente com um erro cru.
#' @noRd
tl_project_missing_collections <- function(caminho, lib) {
  pedidas <- tl_project_collections(caminho)
  pedidas[!vapply(pedidas, function(p) dir.exists(file.path(lib, p)), logical(1))]
}

#' Abre um projeto: adiciona `caminho` aos recentes do estado e sobe o
#' editor (`trama::tr_app(trama::tr_project(caminho))`) num processo R
#' separado, numa porta livre a partir de 8726 (a mesma faixa que
#' `trama::tr_port_default()` usa) — cada projeto aberto na sua porta, o
#' launcher continua noutra. Sem `processx`/`callr`, mesma decisão da task
#' 2.1 para "Abrir editor": `system2(..., wait = FALSE)`.
#'
#' @param caminho Pasta do projeto (já existente).
#' @param lib Biblioteca da release atual.
#' @param abrir_janela Como abrir a URL do editor depois de subir — por
#'   padrão `tl_open_window()`, para poder mockar nos testes.
#' @param executar Wrapper em torno de `system2()`, só para poder mockar nos
#'   testes (mesmo motivo de `tl_install_pkgs()` em `R/install.R`).
#' @noRd
tl_project_open <- function(caminho, lib = tl_lib_dir(tl_state_read()$atual),
                             abrir_janela = tl_open_window, executar = system2) {
  caminho <- normalizePath(caminho, mustWork = TRUE)

  s <- tl_state_read()
  s$recentes <- utils::head(union(caminho, s$recentes), 10)
  tl_state_write(s)

  # Já tem um editor de pé para este projeto (porta registrada e
  # respondendo): só reabre a janela nela, sem subir outro processo por
  # cima — revisão da fase 2, "Abrir" num projeto já aberto duplicava o
  # `Rscript`.
  if (tl_project_aberto(caminho)) {
    porta <- tl_project_port(caminho)
    abrir_janela(sprintf("http://127.0.0.1:%d", porta))
    return(invisible(porta))
  }

  porta <- .tl_porta_livre(.tl_porta_projetos_inicial())
  pid_file <- .tl_pid_file(caminho)
  dir.create(dirname(pid_file), recursive = TRUE, showWarnings = FALSE)
  unlink(pid_file)
  cmd <- sprintf(
    paste0(
      "writeLines(as.character(Sys.getpid()), %s); ",
      ".libPaths(c(%s, .Library)); ",
      "trama::tr_app(trama::tr_project(%s), port = %d, options = list(launch.browser = FALSE))"
    ),
    deparse(pid_file), deparse(lib), deparse(caminho), porta
  )
  rscript <- file.path(R.home("bin"), "Rscript")
  executar(rscript, c("--vanilla", "-e", shQuote(cmd)), wait = FALSE)
  assign(caminho, porta, envir = .tl_processos_projeto)
  assign(caminho, pid_file, envir = .tl_pids_projeto)

  abrir_janela(sprintf("http://127.0.0.1:%d", porta))
  invisible(porta)
}

#' Pasta onde ficam os arquivos de PID dos editores de projeto (um por
#' projeto aberto), dentro de `TRAMA_HOME`. Existe para
#' `tl_project_restart()` conseguir matar o processo do editor sem depender
#' de `processx`/`callr` (decisão já tomada em `tl_project_open()`):
#' `system2(..., wait = FALSE)` não devolve o PID do filho de forma
#' portável, então o próprio processo R do editor grava o `Sys.getpid()`
#' dele nesse arquivo assim que sobe.
#' @noRd
.tl_run_dir <- function() file.path(tl_home(), "run")

#' Slug estável (mesma pasta → mesmo nome de arquivo) a partir do caminho
#' do projeto, só com caracteres seguros de nome de arquivo em qualquer SO.
#' Não precisa ser criptográfico: só identifica "o pid file deste projeto",
#' nunca é comparado entre sessões diferentes de forma sensível.
#' @noRd
.tl_slug <- function(caminho) {
  soma <- sum(utf8ToInt(caminho)) %% 2147483647L
  base <- gsub("[^A-Za-z0-9]+", "-", basename(caminho))
  sprintf("%s-%x", base, soma)
}

#' Arquivo de PID do editor de `caminho`.
#' @noRd
.tl_pid_file <- function(caminho) file.path(.tl_run_dir(), paste0(.tl_slug(caminho), ".pid"))

#' Registro em memória `caminho -> arquivo de pid`, espelho de
#' `.tl_processos_projeto` (que guarda a porta) para o mesmo projeto.
#' @noRd
.tl_pids_projeto <- new.env(parent = emptyenv())

#' PID do editor de `caminho`, lido do arquivo que o próprio processo
#' gravou ao subir — `NA` se nunca foi registrado ou o arquivo ainda não
#' foi escrito (processo subindo) ou está corrompido.
#' @noRd
tl_project_pid <- function(caminho) {
  chave <- normalizePath(caminho, mustWork = FALSE)
  if (!exists(chave, envir = .tl_pids_projeto, inherits = FALSE)) return(NA_integer_)
  arquivo <- get(chave, envir = .tl_pids_projeto)
  if (!file.exists(arquivo)) return(NA_integer_)
  pid <- suppressWarnings(as.integer(trimws(readLines(arquivo, n = 1, warn = FALSE))))
  if (length(pid) != 1 || is.na(pid)) return(NA_integer_)
  pid
}

#' Mata o processo `pid` — `tools::pskill()` no Windows (mesma
#' recomendação da task: sem adicionar `processx`), `tools::pskill()`
#' também serve no Unix (mata com `SIGTERM`). Função à parte só para poder
#' mockar nos testes, sem matar processo de verdade.
#' @noRd
.tl_matar_pid <- function(pid) {
  if (is.na(pid)) return(invisible(FALSE))
  invisible(tools::pskill(pid))
}

#' Reinicia o editor de um projeto já aberto: mata o processo registrado
#' (pelo PID gravado em `.tl_pid_file()`) e sobe um novo, numa porta nova
#' (a antiga pode continuar presa por um instante depois do `pskill`).
#' Usado quando as coleções de um projeto mudam (task "Coleções" da UI) —
#' o editor de pé continua com o registro antigo em memória até reiniciar.
#'
#' Se o projeto não estava aberto (`tl_project_aberto()` é `FALSE`), não
#' faz nada além de limpar o registro: não sobe editor para um projeto que
#' o usuário não tinha aberto.
#'
#' @param caminho Pasta do projeto.
#' @param lib Biblioteca da release atual.
#' @param abrir_janela Ver `tl_project_open()`.
#' @param executar Ver `tl_project_open()`.
#' @param matar Wrapper em torno de `tools::pskill()`, só para mockar nos testes.
#' @noRd
tl_project_restart <- function(caminho, lib = tl_lib_dir(tl_state_read()$atual),
                                abrir_janela = tl_open_window, executar = system2,
                                matar = .tl_matar_pid) {
  caminho <- normalizePath(caminho, mustWork = TRUE)
  if (!tl_project_aberto(caminho)) return(invisible(NULL))

  pid <- tl_project_pid(caminho)
  matar(pid)
  rm(list = caminho, envir = .tl_processos_projeto)
  if (exists(caminho, envir = .tl_pids_projeto, inherits = FALSE)) {
    rm(list = caminho, envir = .tl_pids_projeto)
  }

  # Dá um instante para a porta antiga soltar antes de checar de novo —
  # sem isso, `tl_project_open()` acharia (por uma checagem de socket que
  # ainda não caiu) que o projeto continua aberto na porta velha e só
  # reabriria a janela nela, sem subir o editor novo.
  Sys.sleep(0.2)
  tl_project_open(caminho, lib = lib, abrir_janela = abrir_janela, executar = executar)
}
