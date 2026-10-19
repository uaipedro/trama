#' Extensões do editor
#'
#' Extensão é um pacote de apoio que acrescenta algo ao editor sem ser
#' coleção: não tem blocos e não entra no `trama.json` do projeto, vale para a
#' instalação inteira (o menu GitHub do `trama.github` é o caso de uso). O
#' núcleo não conhece nenhuma pelo nome nem depende do que elas fazem; acha as
#' instaladas pelo DESCRIPTION.
#'
#' Contrato: um pacote `trama.*` com `Config/trama/extension: true` no
#' DESCRIPTION e uma função exportada `trama_extension()` que devolve uma lista
#' com
#'
#' - `js`, `css`: caminhos dentro de `inst/` (como os de coleção), carregados
#'   depois das coleções e antes do editor. O JS é módulo e importa de
#'   `"trama"` (por exemplo `registerToolbarItem`);
#' - `server`: `function(input, output, session, ctx)`, chamada uma vez por
#'   sessão do editor. `ctx$project()` devolve o projeto aberto.
#'
#' Extensão que falha ao carregar é pulada com aviso: o editor sobe sem ela.
#'
#' @param pacotes Pacotes a carregar. O padrão é `getOption("trama.extensions")`
#'   e, sem ela, todo pacote `trama.*` instalado ou carregado que se declara
#'   extensão. `options(trama.extensions = character())` desliga todas.
#' @return Lista nomeada pelo pacote, com `js`, `css`, `server`, `package` e
#'   `version`.
#' @export
tr_extensions <- function(pacotes = getOption("trama.extensions")) {
  if (is.null(pacotes)) pacotes <- .tr_extension_candidatos()
  exts <- list()
  for (p in pacotes) {
    e <- tryCatch(.tr_extension_carregar(p), error = function(e) {
      message(sprintf("trama: extensão %s não carregou: %s", p, conditionMessage(e)))
      NULL
    })
    if (!is.null(e)) exts[[p]] <- e
  }
  exts
}

.tr_extension_candidatos <- function() {
  nomes <- unique(c(loadedNamespaces(), .packages(all.available = TRUE)))
  nomes <- sort(grep("^trama[.]", nomes, value = TRUE))
  nomes[vapply(nomes, .tr_extension_flag, logical(1))]
}

.tr_extension_flag <- function(pkg) {
  v <- suppressWarnings(tryCatch(
    utils::packageDescription(pkg, fields = "Config/trama/extension"),
    error = function(e) NA))
  isTRUE(as.logical(v))
}

.tr_extension_carregar <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("pacote não instalado")
  e <- getExportedValue(pkg, "trama_extension")()
  if (!is.list(e)) stop("`trama_extension()` não devolveu uma lista")
  e$package <- pkg
  e$version <- as.character(utils::packageVersion(pkg))
  e
}

# Dependências htmltools das extensões, na ordem em que foram achadas. Mesmo
# formato das de coleção; o nome leva `ext-` para não colidir com uma coleção.
.tr_extension_deps <- function(exts) {
  unlist(lapply(names(exts), function(p) {
    .tr_dependency_assets(exts[[p]], paste0("ext-", sub("^trama[.]", "", p)))
  }), recursive = FALSE)
}

# Liga o server de cada extensão à sessão. Erro numa não derruba o editor
# nem as outras.
.tr_extension_servers <- function(exts, input, output, session, ctx) {
  for (p in names(exts)) {
    srv <- exts[[p]]$server
    if (!is.function(srv)) next
    tryCatch(srv(input, output, session, ctx), error = function(e) {
      message(sprintf("trama: extensão %s falhou ao iniciar: %s", p, conditionMessage(e)))
    })
  }
  invisible()
}
