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

#' Cria um projeto novo: valida o nome e cria a pasta dentro de
#' `tl_projects_dir()`.
#' @noRd
tl_project_new <- function(nome) {
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
  caminho
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
  cmd <- sprintf(
    paste0(
      ".libPaths(c(%s, .Library)); ",
      "trama::tr_app(trama::tr_project(%s), port = %d, options = list(launch.browser = FALSE))"
    ),
    deparse(lib), deparse(caminho), porta
  )
  rscript <- file.path(R.home("bin"), "Rscript")
  executar(rscript, c("--vanilla", "-e", shQuote(cmd)), wait = FALSE)
  assign(caminho, porta, envir = .tl_processos_projeto)

  abrir_janela(sprintf("http://127.0.0.1:%d", porta))
  invisible(porta)
}
