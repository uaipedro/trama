# A borda da área de estudo: o tipo `spatial/boundary`, o casco convexo e a
# regra de projeção entre a borda e os pontos.
#
# Antes desta versão a borda só existia nos três conjuntos de exemplo: o nó
# `spatial/coordinates` não a expunha, porque "a borda de uma tabela qualquer
# não tem de onde vir". O custo era silencioso — para dado próprio a grade da
# krigagem era o retângulo da extensão, sem recorte, e a guarda de contenção
# nunca disparava.

#' Casco convexo dos pontos, como borda.
#'
#' A opção de borda SEM arquivo: resolve o recorte da grade para quem só tem a
#' tabela, e é defensável como domínio de interpolação — fora do casco toda
#' predição é extrapolação.
#'
#' `sf::st_convex_hull` de pontos COLINEARES devolve `LINESTRING`, não
#' `POLYGON` (medido com sf 1.0.22), então a guarda é obrigatória e mora aqui.
#'
#' @param coords matriz ou data.frame com duas colunas de coordenada.
#' @return matriz n×2 fechada (primeira linha igual à última).
#' @export
tr_spatial_convex_hull <- function(coords) {
  co <- as.matrix(coords)
  if (ncol(co) != 2L || !is.numeric(co)) {
    .tr_spatial_abort("tr_spatial_error_bad_coords",
      "O casco convexo precisa de duas colunas numéricas de coordenada.")
  }
  distintos <- nrow(unique(co))
  if (distintos < 3L) {
    .tr_spatial_abort("tr_spatial_error_bad_border", sprintf(paste(
      "O casco convexo precisa de ao menos três pontos distintos, e há %d.",
      "Use uma borda de arquivo, ou deixe o objeto sem borda."), distintos))
  }
  d <- as.data.frame(co)
  h <- sf::st_convex_hull(sf::st_union(sf::st_as_sf(d, coords = names(d))))
  if (!inherits(sf::st_geometry(h)[[1]], "POLYGON")) {
    .tr_spatial_abort("tr_spatial_error_bad_border", paste(
      "Os pontos são colineares: o casco convexo deles é uma linha, não uma área.",
      "Use uma borda de arquivo, ou deixe o objeto sem borda."))
  }
  .tr_spatial_borda(sf::st_coordinates(h)[, 1:2, drop = FALSE])
}
