#' Projeto: a pasta que reúne documento, store e manifesto de coleções.
#'
#' Existe porque três coisas no insumo eram caminhos fixos no código — o
#' diretório de blocos, o de blueprints e o que o "usos de bloco" varria. Sem
#' um conceito de projeto, "abri e deu tipo de nó desconhecido" não tem
#' diagnóstico: o manifesto é o que permite dizer QUAL coleção falta.
#'
#'   meu-projeto/
#'     trama.json        coleções, executor
#'     flows/*.json      documentos
#'     .trama/store/     artefatos (gitignore)
#' @export
tr_project <- function(root = ".", collections = character(), create = TRUE) {
  root <- normalizePath(root, mustWork = FALSE)
  cfg_path <- file.path(root, "trama.json")
  if (create) .tr_project_dirs(root)
  cfg <- if (file.exists(cfg_path)) jsonlite::fromJSON(cfg_path, simplifyVector = FALSE) else list()
  cols <- if (length(collections)) as.character(collections) else unlist(cfg$collections)
  settings <- .tr_settings(cfg)

  registry <- tr_registry()
  for (cl in cols) tr_use(cl, registry = registry)

  .tr_project_obj(root, registry, cols, settings)
}

#' Cria as pastas de um projeto.
#'
#' `showWarnings = FALSE` cala a única pista que `dir.create` dá, e sem a
#' conferência abaixo uma pasta pai só-leitura passava batido aqui pra estourar
#' adiante, no `writeLines` do manifesto, como `simpleError` sem classe e sem
#' citar caminho nenhum. Quem chama `tr_project_new()` pela interface DIGITOU
#' esse caminho: ele tem que voltar na mensagem. Mesmo motivo do `tr_store`
#' (ver `.tr_atomic`): disco cheio e permissão são reais.
#' @noRd
.tr_project_dirs <- function(root) {
  for (d in c(file.path(root, "flows"), file.path(root, ".trama", "store"),
              file.path(root, "imagens"))) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(d)) {
      rlang::abort(sprintf(
        "Não foi possível criar a pasta '%s'. Verifique a permissão de escrita e o espaço em disco.", d),
        class = "tr_error_project_write")
    }
  }
  invisible(root)
}

#' Monta o objeto `tr_project`.
#'
#' Único de propósito: `tr_project()` e `tr_project_at()` montavam a mesma lista
#' cada um por si, e campo novo lembrado só no primeiro daria um projeto capenga
#' exatamente ao TROCAR de projeto — o caminho que ninguém exercita à toa.
#' @noRd
.tr_project_obj <- function(root, registry, collections, settings) {
  structure(list(
    root = root, registry = registry, settings = settings,
    store = tr_store(file.path(root, ".trama", "store"), project_root = root),
    collections = collections,
    flows_dir = file.path(root, "flows")
  ), class = "tr_project")
}

#' Recusa o que não é um caminho de pasta utilizável.
#'
#' Mesmo motivo de `.tr_project_obj()`: `tr_project_at()` e `.tr_dir_listing()`
#' recebem o caminho da MESMA origem — o que o usuário digitou ou clicou no
#' diálogo — e a cópia já tinha divergido antes de nascer, porque o tratamento
#' do caminho vazio (`"<vazio>"`, que sem isso vira a mensagem sem sujeito
#' "Não é uma pasta: .") foi lembrado só de um lado.
#' @noRd
.tr_check_dir <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !dir.exists(path)) {
    alvo <- paste(as.character(path), collapse = ", ")
    rlang::abort(sprintf("Não é uma pasta: %s.", if (nzchar(alvo)) alvo else "<vazio>"),
                 class = "tr_error_bad_root")
  }
  invisible(path)
}

#' Uma pasta é projeto quando tem manifesto.
#'
#' Mesmo motivo de `.tr_check_dir()`: quem MARCA o que é projeto no diálogo
#' (`.tr_dir_listing()`) e quem RECUSA abrir o que não é (`.tr_check_project()`)
#' têm que concordar. Predicado duplicado aqui seria o diálogo oferecendo uma
#' pasta que o servidor recusa, ou o contrário.
#' @noRd
.tr_eh_projeto <- function(path) file.exists(file.path(path, "trama.json"))

#' Recusa ABRIR o que não é um projeto.
#'
#' `tr_project_at()` tolera pasta sem manifesto de propósito, por coerência com
#' `tr_project()` — mas tolerar ali significava que apontar "Abrir" para
#' `~/minhas-fotos` trocava de projeto com sucesso e deixava `.trama/` e `flows/`
#' lá dentro. O diálogo só oferece "Abrir" para pasta que é projeto; esta é a
#' mesma exigência do lado de cá, porque garantia que só o cliente faz não é
#' garantia.
#' @noRd
.tr_check_project <- function(root) {
  .tr_check_dir(root)
  if (!.tr_eh_projeto(root)) {
    rlang::abort(sprintf(
      "'%s' não é um projeto trama: não tem trama.json. Escolha uma pasta de projeto, ou crie um projeto novo.",
      root), class = "tr_error_not_project")
  }
  invisible(root)
}

#' Caminho de um projeto novo, a partir do nome digitado.
#'
#' O nome vem do cliente e ia cru para o `file.path`: `""` transformava em
#' projeto a pasta que o usuário estava só navegando (Documentos, home), `".."`
#' punha o projeto fora dela e `"a/b"` criava dois níveis — tudo sem um aviso.
#' A regra mora aqui, e não no transporte, porque "nome de projeto" é conceito
#' de projeto; o transporte é barramento.
#' @noRd
.tr_project_path <- function(base, nome) {
  .tr_check_dir(base)
  ok <- is.character(nome) && length(nome) == 1L && !is.na(nome)
  # O nome vira PASTA, e espaço nas bordas dá uma que o usuário não consegue
  # redigitar e que o diálogo mostra igual à sem espaço.
  limpo <- if (ok) trimws(nome) else ""
  seps <- unique(c("/", "\\", .Platform$file.sep))
  escapa <- limpo %in% c(".", "..") ||
    any(vapply(seps, function(s) grepl(s, limpo, fixed = TRUE), TRUE))
  if (!ok || !nzchar(limpo) || escapa) {
    rlang::abort(paste0(
      "Nome de projeto inválido. Digite o nome da pasta a criar dentro de '", base,
      "': um nome só, sem barras e sem ser '.' ou '..'."),
      class = "tr_error_bad_name")
  }
  file.path(base, limpo)
}

#' Abre um projeto REUSANDO um registry já carregado.
#'
#' É o que permite trocar de projeto com o editor em pé. `tr_project()` monta um
#' registry novo, carregando cada coleção do manifesto — e é isso que não se pode
#' fazer numa página que já está aberta: o JS e o CSS de cada coleção entram como
#' `htmlDependency` quando a UI é montada (`tr_ui()`), uma vez só. Registry novo
#' com coleção nova no R e nenhum renderer no navegador seria um card verde sem
#' desenho nenhum.
#'
#' Daí a recusa ser a regra, e não um remendo: se o manifesto pede coleção que a
#' página não tem, isto aborta NOMEANDO-A, antes de criar pasta ou tocar em
#' qualquer estado. É o diagnóstico para o qual o manifesto existe.
#'
#' A comparação é contra o PACOTE (`$package`), não contra o id da coleção: o
#' manifesto guarda `trama.data` e o registry indexa por `data`, e os dois só
#' coincidem por acidente (ver `tr_use()`).
#' @export
tr_project_at <- function(root, registry) {
  .tr_check_dir(root)
  # Simétrico à checagem do `root`: registry NULL passava direto e produzia um
  # `tr_project` sem registro, que só quebra lá na frente, ao executar — longe
  # daqui, e sem dizer que a chamada é que estava errada.
  if (!inherits(registry, "tr_registry")) {
    rlang::abort("tr_project_at() precisa do registry da página, o de tr_registry().",
                 class = "tr_error_bad_root")
  }
  root <- normalizePath(root, mustWork = TRUE)

  cfg_path <- file.path(root, "trama.json")
  cfg <- if (file.exists(cfg_path)) jsonlite::fromJSON(cfg_path, simplifyVector = FALSE) else list()
  cols <- unlist(cfg$collections) %||% character()

  disponiveis <- .tr_registry_packages(registry)
  faltando <- setdiff(cols, disponiveis)
  if (length(faltando)) {
    rlang::abort(sprintf(
      "Este editor não tem a(s) coleção(ões) %s, que o projeto pede. Suba o trama com ela(s) para abrir '%s'.",
      paste(faltando, collapse = ", "), basename(root)),
      class = "tr_error_missing_collection")
  }
  # Tema inválido também é recusa antes de tocar no disco: trocar de projeto e
  # só descobrir o manifesto quebrado no primeiro gráfico deixaria pastas
  # criadas num projeto que nem abriu.
  settings <- .tr_settings(cfg)

  .tr_project_dirs(root)

  .tr_project_obj(root, registry, cols, settings)
}

#' Cria um projeto novo: as pastas e o manifesto.
#'
#' `tr_project()` LÊ o `trama.json` e nunca o escreve — os manifestos do
#' repositório foram escritos à mão. Fazer `create = TRUE` passar a gravá-lo
#' seria pior que a lacuna: `tr_project()` é chamado sobre projeto que JÁ existe
#' o tempo todo, e ele reescreveria o manifesto alheio com as coleções que por
#' acaso lhe passaram, em silêncio. Criar é outro verbo.
#'
#' `collections` são nomes de PACOTE, como no manifesto. Quem chama pela
#' interface passa as coleções que a página tem — é a única escolha que produz
#' projeto abrível ali mesmo, sem recarregar.
#' @export
tr_project_new <- function(root, collections = character()) {
  root <- normalizePath(root, mustWork = FALSE)
  cfg_path <- file.path(root, "trama.json")
  if (file.exists(cfg_path)) {
    rlang::abort(sprintf("Já existe um projeto em '%s'. Escolha outra pasta ou outro nome.", root),
                 class = "tr_error_project_exists")
  }
  .tr_project_dirs(root)
  # `auto_unbox = FALSE` de propósito: uma coleção só tem que sair como array
  # `["trama.data"]`, e não como a string `"trama.data"` — `unlist(cfg$collections)`
  # do lado da leitura devolve as duas formas igual, mas um humano que abra o
  # arquivo veria dois formatos para a mesma coisa.
  #
  # Reclassificado porque o erro de `writeLines` chega como `simpleError` com
  # "não é possível abrir a conexão" e mais nada: a interface não tem como
  # distingui-lo de um bug, e o usuário não fica sabendo qual caminho falhou.
  tryCatch(
    writeLines(jsonlite::toJSON(list(collections = as.character(collections)),
                                auto_unbox = FALSE, pretty = TRUE), cfg_path),
    error = function(e) {
      rlang::abort(sprintf(
        "Não foi possível gravar o manifesto em '%s'. Verifique a permissão de escrita e o espaço em disco.",
        cfg_path), class = "tr_error_project_write", parent = e)
    })
  invisible(root)
}

#' Lê o flow `name` do projeto; documento vazio se o arquivo ainda não existe.
#' @export
tr_project_flow <- function(project, name = "main") {
  p <- file.path(project$flows_dir, paste0(name, ".json"))
  if (file.exists(p)) tr_doc_read(p) else tr_doc()
}

#' Grava o flow `name` do projeto em `flows/<name>.json`.
#' @export
tr_project_save <- function(project, doc, name = "main") {
  dir.create(project$flows_dir, recursive = TRUE, showWarnings = FALSE)
  tr_doc_write(doc, file.path(project$flows_dir, paste0(name, ".json")))
}

#' @export
print.tr_project <- function(x, ...) {
  cat(sprintf("<tr_project> %s\n  coleções: %s\n", x$root,
              paste(x$collections, collapse = ", ")))
  invisible(x)
}

#' Uma pasta, do jeito que o diálogo de abrir precisa ver.
#'
#' O navegador NÃO tem como escolher uma pasta do disco do R: `webkitdirectory`
#' manda o conteúdo dos arquivos para o navegador e nunca devolve um caminho. Por
#' isso quem lista é o servidor, e por isso `parent` vem daqui — só quem tem o
#' disco sabe se há para onde subir (`dirname("/")` é `"/"`, e um ".." ali seria
#' um botão que recarrega a mesma pasta).
#'
#' Oculto fica de fora: `.trama` é o store do próprio projeto e `.git` é o
#' repositório. Nenhum dos dois é lugar de projeto, e listá-los é ruído entre o
#' usuário e a pasta que ele procura.
#'
#' Pasta ilegível não é erro: `list.dirs()` devolve vazio, e o diálogo mostra uma
#' pasta sem filhas — que é a verdade do ponto de vista de quem está olhando.
#' @noRd
.tr_dir_listing <- function(path) {
  .tr_check_dir(path)
  path <- normalizePath(path, mustWork = TRUE)

  dirs <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  # `sort()` colaciona no locale da sessão, e aqui isso é o acerto e não o
  # descuido: esta ordem só serve para o olho de quem lê o diálogo, e é a mesma
  # que o gerenciador de arquivos dele usa — no C puro "Zeta" viria antes de
  # "alfa" e "área" iria parar depois de "azul".
  dirs <- sort(dirs[nzchar(dirs) & !startsWith(dirs, ".")])
  pai <- dirname(path)

  list(
    path = path,
    parent = if (identical(pai, path)) NULL else pai,
    project = .tr_eh_projeto(path),
    entries = lapply(dirs, function(d) list(nome = d, projeto = .tr_eh_projeto(file.path(path, d))))
  )
}
