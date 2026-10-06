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
    types = list(spatial_points_type(), spatial_variogram_type(), spatial_model_type()),
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
    nodes = c(.tr_spatial_nos_fonte(), .tr_spatial_nos_variograma()),
    datasets = list(
      trama::tr_dataset(
        "trama.spatial", "milho_pr", "Rendimento do milho no Paraná",
        node = "spatial/example", params = list(dataset = "milho_pr"),
        descricao = "389 sedes municipais, com o rendimento da soja como covariável e a borda do estado.",
        fonte = "IBGE/SIDRA, tabela 5457 (Produção Agrícola Municipal), 2023; sedes municipais e malha estadual do IBGE.",
        n = 389L, temas = c("agricultura", "solo"),
        licenca = "Dados abertos do IBGE: uso, redistribuição e reuso livres, citada a fonte.",
        url = "https://sidra.ibge.gov.br/tabela/5457"),
      trama::tr_dataset(
        "trama.spatial", "cafe_mg", "Rendimento do café em Minas Gerais",
        node = "spatial/example", params = list(dataset = "cafe_mg"),
        descricao = "496 sedes municipais e a borda do estado; dependência espacial moderada.",
        fonte = "IBGE/SIDRA, tabela 5457 (Produção Agrícola Municipal), 2023; sedes municipais e malha estadual do IBGE.",
        n = 496L, temas = c("agricultura", "solo"),
        licenca = "Dados abertos do IBGE: uso, redistribuição e reuso livres, citada a fonte.",
        url = "https://sidra.ibge.gov.br/tabela/5457"),
      trama::tr_dataset(
        "trama.spatial", "milho_se", "Rendimento do milho em Sergipe",
        node = "spatial/example", params = list(dataset = "milho_se"),
        descricao = "68 sedes municipais e a borda do estado; conjunto pequeno, para exemplo rápido.",
        fonte = "IBGE/SIDRA, tabela 5457 (Produção Agrícola Municipal), 2023; sedes municipais e malha estadual do IBGE.",
        n = 68L, temas = c("agricultura", "solo"),
        licenca = "Dados abertos do IBGE: uso, redistribuição e reuso livres, citada a fonte.",
        url = "https://sidra.ibge.gov.br/tabela/5457")
    )
  )
}
