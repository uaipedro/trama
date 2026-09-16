#' Agrupa tipos, nós, adaptadores e categorias de um domínio.
#'
#' Uma coleção é o que um pacote de domínio exporta. `js` aponta pra um módulo
#' ES em `inst/` que registra renderers e widgets no front — é o que permite
#' visualização nova sem tocar no núcleo.
#'
#' `label` é o nome que a coleção mostra na aba da paleta; sem ele, a aba
#' escreve o `id`, que serve mas não explica.
#' @param js Caminho, relativo ao `inst/` do pacote, do módulo ES da coleção.
#'   Opcional. Os arquivos vizinhos no mesmo diretório também são servidos, pro
#'   módulo poder importá-los por caminho relativo.
#' @param css Caminho, relativo ao `inst/` do pacote, de uma folha de estilo da
#'   coleção (as classes que os renderers dela usam). Opcional. Entra na página
#'   DEPOIS do `trama.css` do núcleo, então pode refinar regra do núcleo com a
#'   mesma especificidade — e é por isso mesmo que deve prefixar as próprias
#'   classes, pra não refinar sem querer.
#' @examples
#' minha <- tr_collection(
#'   id = "exemplo", version = "1.0.0", label = "Exemplo",
#'   categories = list(tr_category("basico", "Básico")),
#'   types = list(tr_type("exemplo/num", label = "Número")),
#'   nodes = list(
#'     tr_node("exemplo/dobro", fn = function(x) x * 2,
#'             description = "Dobra o número de entrada.",
#'             category = "basico",
#'             inputs = list(x = "exemplo/num"),
#'             outputs = list(out = "exemplo/num"))
#'   )
#' )
#'
#' reg <- tr_registry()
#' tr_use(minha, registry = reg)
#' names(reg$nodes)
#' @export
tr_collection <- function(id, version = "0.0.0", label = id, types = list(),
                          nodes = list(), adapters = list(), categories = list(),
                          js = NULL, css = NULL) {
  if (!grepl("^[a-z][a-z0-9_]*$", id)) {
    rlang::abort(.tr_msg("collection.bad_id", id), class = "tr_error_bad_id")
  }
  # Checado aqui, e não quando o app sobe: `tr_dependency_collection()` só
  # roda ao montar a UI, e um caminho errado ali some em silêncio (o asset
  # simplesmente não carrega). Absoluto é recusado porque o caminho é resolvido
  # por `system.file()` dentro do pacote — um `/home/...` funcionaria na máquina
  # do autor e em nenhuma outra.
  .tr_check_asset(js, "js", id)
  .tr_check_asset(css, "css", id)
  # Tudo que a coleção declara tem que viver sob o namespace dela. Sem esta
  # checagem, uma coleção poderia registrar `outra/coisa` e o conflito só
  # apareceria como comportamento estranho na coleção alheia.
  for (t in types) if (.tr_collection_of(t$id) != id) {
    rlang::abort(.tr_msg("collection.foreign_type", id, t$id),
                 class = "tr_error_foreign_id")
  }
  for (n in nodes) if (.tr_collection_of(n$id) != id) {
    rlang::abort(.tr_msg("collection.foreign_node", id, n$id),
                 class = "tr_error_foreign_id")
  }
  structure(list(id = id, label = label, version = version, types = types,
                 nodes = nodes, adapters = adapters, categories = categories,
                 js = js, css = css),
            class = "tr_collection")
}

#' @noRd
.tr_check_asset <- function(path, arg, id) {
  if (is.null(path)) return(invisible())
  ok <- is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) &&
    !grepl("^(/|~|[A-Za-z]:)", path)
  if (!ok) {
    rlang::abort(.tr_msg("collection.bad_asset_relative", id, arg), class = "tr_error_bad_asset")
  }
  # O diretório do asset é servido INTEIRO (`all_files = TRUE` em
  # `tr_dependency_collection()`), então o caminho decide o que vai pro
  # navegador. `..` sobe pra fora da pasta pensada pra isso; a barra invertida
  # é separador só no Windows, e o mesmo caminho acharia arquivos diferentes
  # conforme a máquina; e um nome solto tem `dirname()` ".", que no
  # `system.file()` é a raiz do pacote instalado — tudo dele viraria público.
  if (any(strsplit(path, "/", fixed = TRUE)[[1]] == "..")) {
    rlang::abort(.tr_msg("collection.bad_asset_parent", id, arg, path), class = "tr_error_bad_asset")
  }
  if (grepl("\\", path, fixed = TRUE)) {
    rlang::abort(.tr_msg("collection.bad_asset_backslash", id, arg, path), class = "tr_error_bad_asset")
  }
  if (dirname(path) == ".") {
    rlang::abort(.tr_msg("collection.bad_asset_subdir", id, arg, path), class = "tr_error_bad_asset")
  }
  invisible()
}

#' Categoria: só agrupamento visual na paleta. Cor e rótulo vêm daqui e o
#' front nunca conhece nenhuma delas por nome.
#' @export
tr_category <- function(id, label, color = "#6366f1") {
  list(id = id, label = label, color = color)
}
