# Documentação dos objetos de `data/`. Gerados por `data-raw/gerar.R`.

#' Rendimento do milho nos municípios do Paraná, 2023.
#'
#' Rendimento médio do milho em grão em 389 municípios do Paraná, com o
#' rendimento da soja como covariável e a borda do estado. Dependência espacial
#' forte, com tendência de larga escala.
#'
#' @format Lista com `dados` (data.frame de 389 linhas: `code_muni`,
#'   `municipio`, `leste` e `norte` em metros, `milho_kg_ha`, `soja_kg_ha`) e
#'   `borda` (matriz fechada n x 2, em metros).
#' @source IBGE, Sistema IBGE de Recuperação Automática (SIDRA), tabela 5457,
#'   Produção Agrícola Municipal, variável 112, 2023
#'   (<https://sidra.ibge.gov.br/tabela/5457>); sedes municipais e malha
#'   estadual do IBGE, projetadas em SIRGAS 2000 / UTM 22S (EPSG 31982).
#' @docType data
#' @name spatial_milho_pr
#' @keywords datasets
NULL

#' Rendimento do café nos municípios de Minas Gerais, 2023.
#'
#' Rendimento médio do café em grão (total) em 496 municípios de Minas Gerais,
#' com a borda do estado. Dependência espacial moderada.
#'
#' @format Lista com `dados` (data.frame de 496 linhas: `code_muni`,
#'   `municipio`, `leste` e `norte` em metros, `cafe_kg_ha`) e `borda` (matriz
#'   fechada n x 2, em metros).
#' @source IBGE, Sistema IBGE de Recuperação Automática (SIDRA), tabela 5457,
#'   Produção Agrícola Municipal, variável 112, 2023
#'   (<https://sidra.ibge.gov.br/tabela/5457>); sedes municipais e malha
#'   estadual do IBGE, projetadas em SIRGAS 2000 / UTM 23S (EPSG 31983).
#' @docType data
#' @name spatial_cafe_mg
#' @keywords datasets
NULL

#' Rendimento do milho nos municípios de Sergipe, 2023.
#'
#' Rendimento médio do milho em grão em 68 municípios de Sergipe, com a borda
#' do estado. Conjunto pequeno, para exemplo rápido.
#'
#' @format Lista com `dados` (data.frame de 68 linhas: `code_muni`,
#'   `municipio`, `leste` e `norte` em metros, `milho_kg_ha`) e `borda` (matriz
#'   fechada n x 2, em metros).
#' @source IBGE, Sistema IBGE de Recuperação Automática (SIDRA), tabela 5457,
#'   Produção Agrícola Municipal, variável 112, 2023
#'   (<https://sidra.ibge.gov.br/tabela/5457>); sedes municipais e malha
#'   estadual do IBGE, projetadas em SIRGAS 2000 / UTM 24S (EPSG 31984).
#' @docType data
#' @name spatial_milho_se
#' @keywords datasets
NULL
