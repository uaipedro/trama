# A coleção `spatial`: o registro dos tipos, das categorias e dos nós.
#
# As declarações dos nós moram em `R/nos_*.R`, por aba, como na `sampling`.

#' A coleção `spatial`.
#'
#' Carrega DEPOIS de `trama.data` e `trama.view`: as portas usam `data/table` e
#' `view/plot`, e o registro recusa porta com tipo desconhecido.
#'
#' Os ids de categoria têm prefixo (`espacial_*`) porque o registro de
#' categorias é GLOBAL: uma categoria `explorar` aqui sobrescreveria em silêncio
#' a de outra coleção que usasse o mesmo id.
#' @export
trama_collection <- function() {
  trama::tr_collection(
    id = "spatial", version = "0.1.0", label = "Geoestatística",
    js = "trama/index.js", css = "trama/spatial.css",
    types = list(spatial_points_type()),
    adapters = .tr_spatial_adapters(),
    # As abas seguem o CAMINHO de uma análise geoestatística: declarar o objeto
    # espacial, olhar, medir a dependência, modelá-la, e só então interpolar.
    categories = list(
      trama::tr_category("espacial_fonte",      "Fonte", role = "origem"),
      trama::tr_category("espacial_preparar",   "Preparar", role = "preparacao"),
      trama::tr_category("espacial_explorar",   "Explorar", role = "inspecao"),
      trama::tr_category("espacial_variograma", "Variograma", role = "ajuste"),
      trama::tr_category("espacial_predizer",   "Predizer", role = "ajuste")
    ),
    nodes = list()
  )
}
