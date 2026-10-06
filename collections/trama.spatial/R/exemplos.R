# Os conjuntos de exemplo, versionados em `data/`.
#
# PROCEDÊNCIA E LICENÇA (conferidas na fonte em 2026-10-06):
#
# Todos vêm do IBGE, de uma fonte só: SIDRA, tabela 5457 (Produção Agrícola
# Municipal), variável 112 (rendimento médio da produção, em kg/ha), ano 2023.
# As coordenadas são as sedes municipais do IBGE (2010) e as bordas são as
# fronteiras estaduais do IBGE (2020), ambas projetadas em SIRGAS 2000 / UTM.
#
# Licença: dados abertos do IBGE. Reprodução, redistribuição e reuso
# permitidos, com exigência de CITAÇÃO DA FONTE — que é cumprida aqui, na
# documentação de cada base e no catálogo de bases da coleção.
#
# O pipeline que os gerou está em `data-raw/gerar.R`. O pacote não depende de
# `sidrar` nem de `geobr`: eles baixam, o que se distribui são os números.
#
# - spatial_milho_pr: rendimento do milho em grão em 389 sedes municipais do
#   Paraná, com o rendimento da soja como covariável e a borda do estado.
#   Dependência espacial forte, com tendência de larga escala.
# - spatial_cafe_mg: rendimento do café em grão em 496 sedes municipais de
#   Minas Gerais, com a borda do estado. Dependência moderada.
# - spatial_milho_se: rendimento do milho em 68 sedes municipais de Sergipe,
#   com a borda. Conjunto pequeno, para exemplo rápido.

.TR_SPATIAL_EXEMPLOS <- c("milho_pr", "cafe_mg", "milho_se")

#' Um conjunto de exemplo da coleção, já como objeto espacial.
#'
#' Dados do IBGE (SIDRA, tabela 5457, Produção Agrícola Municipal de 2023),
#' com as coordenadas das sedes municipais e a borda do estado.
#' @param dataset `"milho_pr"`, `"cafe_mg"` ou `"milho_se"`.
#' @return um objeto espacial (`spatial/points`).
#' @export
tr_spatial_example <- function(dataset = "milho_pr") {
  if (!is.character(dataset) || length(dataset) != 1L ||
      !dataset %in% .TR_SPATIAL_EXEMPLOS) {
    .tr_spatial_abort("tr_spatial_error_bad_option", sprintf(
      "Conjunto: escolha um de %s.", paste(.TR_SPATIAL_EXEMPLOS, collapse = ", ")))
  }
  switch(dataset,
    milho_pr = tr_spatial_coordinates(
      spatial_milho_pr$dados, x = "leste", y = "norte", variavel = "milho_kg_ha",
      covariaveis = "soja_kg_ha", crs = "31982", unidade = "m",
      nome = "Rendimento do milho — Paraná", borda = spatial_milho_pr$borda),
    cafe_mg = tr_spatial_coordinates(
      spatial_cafe_mg$dados, x = "leste", y = "norte", variavel = "cafe_kg_ha",
      crs = "31983", unidade = "m",
      nome = "Rendimento do café — Minas Gerais", borda = spatial_cafe_mg$borda),
    milho_se = tr_spatial_coordinates(
      spatial_milho_se$dados, x = "leste", y = "norte", variavel = "milho_kg_ha",
      crs = "31984", unidade = "m",
      nome = "Rendimento do milho — Sergipe", borda = spatial_milho_se$borda))
}
