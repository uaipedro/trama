#' Plugins do editor
#'
#' Plugin é um pacote que acrescenta algo ao editor sem ser
#' coleção: não tem blocos e não entra no `trama.json` do projeto, vale para a
#' instalação inteira (o menu GitHub do `trama.github` é o caso de uso). O
#' núcleo não conhece nenhuma pelo nome nem depende do que elas fazem; acha as
#' instaladas pelo DESCRIPTION.
#'
#' Contrato: um pacote `trama.*` com `Config/trama/plugin: true` no
#' DESCRIPTION e uma função exportada `trama_plugin()` que devolve uma lista
#' com
#'
#' - `js`, `css`: caminhos dentro de `inst/` (como os de coleção), carregados
#'   depois das coleções e antes do editor. O JS é módulo e importa de
#'   `"trama"` (por exemplo `registerToolbarItem`);
#' - `server`: `function(input, output, session, ctx)`, chamada uma vez por
#'   sessão do editor. `ctx$project()` devolve o projeto aberto e
#'   `ctx$recarregar()` relê do disco o projeto aberto (fluxo, temas,
#'   execução), como ao abri-lo; o histórico de desfazer recomeça.
#'   `ctx$doc()` e `ctx$fluxo()` dão o documento na tela e o nome do fluxo;
#'   `ctx$servir(pasta, nome)` serve uma pasta pelo próprio editor e devolve
#'   a URL relativa (`trama-acao-<nome>/`), como nas ações de card.
#'
#' No navegador, além de `registerToolbarItem`, o plugin pode acrescentar
#' itens ao menu de contexto de um card com `registerCardMenuItem` (ver
#' `inst/www/menu-card.js`).
#'
#' Plugin que falha ao carregar é pulada com aviso: o editor sobe sem ela.
#'
#' @param pacotes Pacotes a carregar. O padrão é `getOption("trama.plugins")`
#'   e, sem ela, todo pacote `trama.*` instalado ou carregado que se declara
#'   plugin. `options(trama.plugins = character())` desliga todos.
#' @return Lista nomeada pelo pacote, com `js`, `css`, `server`, `package` e
#'   `version`.
#' @export
tr_plugins <- function(pacotes = getOption("trama.plugins")) {
  if (is.null(pacotes)) pacotes <- .tr_plugin_candidatos()
  plugs <- list()
  for (p in pacotes) {
    e <- tryCatch(.tr_plugin_carregar(p), error = function(e) {
      message(sprintf("trama: plugin %s não carregou: %s", p, conditionMessage(e)))
      NULL
    })
    if (!is.null(e)) plugs[[p]] <- e
  }
  plugs
}

#' Coleções `trama.*` entre `nomes`: exportam `trama_collection()` e não são
#' plugin. Só o prefixo não basta: plugins (`trama.github`) e o launcher
#' também são `trama.*`, e listá-los como coleção derruba o editor (#18).
#' @noRd
.tr_colecoes_instaladas <- function(nomes = rownames(utils::installed.packages())) {
  nomes <- sort(unique(grep("^trama[.]", nomes, value = TRUE)))
  nomes[vapply(nomes, .tr_e_colecao, logical(1))]
}

# Lê o NAMESPACE do disco em vez de carregar o pacote: listar não deve
# custar o load de cada coleção.
.tr_e_colecao <- function(pkg, caminho = find.package(pkg, quiet = TRUE)) {
  if (.tr_plugin_flag(pkg) || length(caminho) != 1L) return(FALSE)
  ns <- file.path(caminho, "NAMESPACE")
  file.exists(ns) && any(grepl("trama_collection", readLines(ns, warn = FALSE), fixed = TRUE))
}

.tr_plugin_candidatos <- function() {
  nomes <- unique(c(loadedNamespaces(), .packages(all.available = TRUE)))
  nomes <- sort(grep("^trama[.]", nomes, value = TRUE))
  nomes[vapply(nomes, .tr_plugin_flag, logical(1))]
}

.tr_plugin_flag <- function(pkg) {
  v <- suppressWarnings(tryCatch(
    utils::packageDescription(pkg, fields = "Config/trama/plugin"),
    error = function(e) NA))
  isTRUE(as.logical(v))
}

.tr_plugin_carregar <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("pacote não instalado")
  e <- getExportedValue(pkg, "trama_plugin")()
  if (!is.list(e)) stop("`trama_plugin()` não devolveu uma lista")
  e$package <- pkg
  e$version <- as.character(utils::packageVersion(pkg))
  e
}

# Dependências htmltools dos plugins, na ordem em que foram achados. Mesmo
# formato das de coleção; o nome leva `plugin-` para não colidir com uma coleção.
.tr_plugin_deps <- function(plugs) {
  unlist(lapply(names(plugs), function(p) {
    .tr_dependency_assets(plugs[[p]], paste0("plugin-", sub("^trama[.]", "", p)))
  }), recursive = FALSE)
}

# Liga o server de cada plugin à sessão. Erro num não derruba o editor
# nem os outros.
.tr_plugin_servers <- function(plugs, input, output, session, ctx) {
  for (p in names(plugs)) {
    srv <- plugs[[p]]$server
    if (!is.function(srv)) next
    tryCatch(srv(input, output, session, ctx), error = function(e) {
      message(sprintf("trama: plugin %s falhou ao iniciar: %s", p, conditionMessage(e)))
    })
  }
  invisible()
}
