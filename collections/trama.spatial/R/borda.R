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

.TR_SPATIAL_CAMPOS_BORDA <- c("poligono", "crs", "area", "n_vertices",
                              "n_aneis_descartados", "fonte", "rotulo", "nota")

#' Objeto de borda (`spatial/boundary`).
#'
#' @param poligono matriz de duas colunas com o contorno; é fechada aqui.
#' @param crs objeto `sf::crs`, ou `NA` para plano arbitrário.
#' @param fonte texto: arquivo e camada de origem, ou como foi construída.
#' @param rotulo título nos cards.
#' @param nota texto da nota do card.
#' @param n_aneis_descartados anéis internos perdidos na normalização.
#' @return objeto `tr_spatial_boundary`.
#' @export
tr_spatial_boundary_obj <- function(poligono, crs, fonte, rotulo, nota,
                                    n_aneis_descartados = 0L) {
  pol <- .tr_spatial_borda(poligono)
  area <- as.numeric(sf::st_area(sf::st_sfc(sf::st_polygon(list(unname(pol))))))
  structure(list(poligono = pol, crs = crs, area = area,
                 n_vertices = nrow(pol),
                 n_aneis_descartados = as.integer(n_aneis_descartados),
                 fonte = as.character(fonte), rotulo = as.character(rotulo),
                 nota = as.character(nota)),
            class = "tr_spatial_boundary")
}

#' A borda na projeção dos pontos.
#'
#' Cinco casos, e a razão de cada um está na ajuda do bloco `spatial/boundary`:
#' mesmo CRS usa direto; CRS diferente reprojeta; borda em GRAU com pontos
#' projetados reprojeta (a malha do IBGE vem em 4674, e a recusa de grau vale
#' para os pontos, não para o recorte); pontos sem CRS aceita no mesmo plano,
#' com nota; borda sem CRS e pontos com CRS **recusa**, porque não há como saber
#' em que plano a borda está.
#'
#' A nota viaja no atributo `nota` do resultado, e quem chama a junta à nota do
#' objeto espacial.
#' @noRd
.tr_spatial_borda_crs <- function(borda, crs_pontos) {
  tem_b <- inherits(borda$crs, "crs") && !is.na(borda$crs)
  tem_p <- inherits(crs_pontos, "crs") && !is.na(crs_pontos)
  if (!tem_b && tem_p) {
    .tr_spatial_abort("tr_spatial_error_crs_mismatch", sprintf(paste(
      "A borda não declara projeção e os pontos estão em %s: não há como saber",
      "em que plano a borda está. Declare o CRS na leitura da borda, ou tire o",
      "CRS dos pontos."), crs_pontos$input))
  }
  if (!tem_p) {
    out <- borda$poligono
    attr(out, "nota") <- paste("Borda usada no mesmo plano dos pontos, que estão",
                               "sem projeção declarada.")
    return(out)
  }
  if (sf::st_crs(borda$crs) == sf::st_crs(crs_pontos)) return(borda$poligono)
  g <- sf::st_transform(
    sf::st_sfc(sf::st_polygon(list(unname(borda$poligono))), crs = borda$crs),
    crs_pontos)
  out <- .tr_spatial_borda(sf::st_coordinates(g)[, 1:2, drop = FALSE])
  attr(out, "nota") <- sprintf("Borda reprojetada de %s para %s.",
                               sf::st_crs(borda$crs)$input,
                               sf::st_crs(crs_pontos)$input)
  out
}
