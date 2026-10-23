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

# Valores do param `borda` do nó `spatial/coordinates`. O casco convexo é a
# opção que não precisa de arquivo.
.TR_SPATIAL_BORDA_MODOS <- c("nenhuma", "casco convexo dos pontos")

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

.tr_spatial_borda_conferir <- function(x) {
  .tr_spatial_guard(x, "tr_spatial_boundary", .TR_SPATIAL_CAMPOS_BORDA,
                    "tr_spatial_error_not_a_boundary", "uma borda")
}

#' O que o card da borda mostra.
#' @noRd
.tr_spatial_borda_preview <- function(x) {
  list(rotulo = x$rotulo, fonte = x$fonte, area = signif(x$area, 6),
       n_vertices = x$n_vertices, n_aneis = x$n_aneis_descartados,
       crs = if (inherits(x$crs, "crs")) x$crs$input else NULL,
       vertices = lapply(seq_len(nrow(x$poligono)), function(i) {
         list(x = x$poligono[i, 1], y = x$poligono[i, 2])
       }),
       nota = .tr_spatial_nulo(x$nota))
}

spatial_boundary_type <- function() {
  .tr_spatial_rds_type("spatial/boundary", "Borda", "#166534",
                       .tr_spatial_borda_conferir,
                       function(x, ctx) trama::tr_preview(
                         "spatial/boundary", data = .tr_spatial_borda_preview(x)),
                       report = tr_spatial_report_boundary)
}

#' Borda -> tabela: uma linha por vértice, na ordem do contorno.
#' @noRd
.tr_spatial_borda_tabela <- function(x) {
  .tr_spatial_borda_conferir(x)
  data.frame(vertice = seq_len(nrow(x$poligono)),
             x = unname(x$poligono[, 1]), y = unname(x$poligono[, 2]))
}

#' Borda a partir de uma geometria do `sf`: dissolve, conta anéis, normaliza.
#'
#' Uma malha estadual costuma vir como vários polígonos (ilhas, enclaves), e o
#' tipo guarda um anel só. Dissolve com `st_union`, fica com o contorno de maior
#' área, e **conta** o que descartou: anel interno perdido em silêncio faria a
#' grade da krigagem cobrir o que a borda excluía.
#'
#' @param g geometria ou `sf` de polígonos.
#' @param fonte texto de procedência, para o card.
#' @param rotulo título; vazio usa o da fonte.
#' @param nota nota adicional, concatenada às que este helper gera.
#' @noRd
.tr_spatial_boundary_de_sf <- function(g, fonte, rotulo = "", nota = "") {
  un <- sf::st_union(sf::st_geometry(g))
  pols <- suppressWarnings(sf::st_cast(un, "POLYGON", warn = FALSE))
  if (!length(pols)) {
    .tr_spatial_abort("tr_spatial_error_bad_border",
      "A geometria não tem nenhum polígono utilizável depois de dissolvida.")
  }
  aneis <- sum(vapply(pols, function(p) max(0L, length(p) - 1L), 0L))
  areas <- vapply(pols, function(p) {
    as.numeric(sf::st_area(sf::st_sfc(sf::st_polygon(p[1]))))
  }, 0)
  maior <- pols[[which.max(areas)]]
  n <- c(if (aneis > 0L) sprintf(
           "%d anel interno descartado: a borda é o contorno externo.", aneis),
         if (length(pols) > 1L) sprintf(
           "%d polígonos dissolvidos; usado o de maior área.", length(pols)),
         nota)
  tr_spatial_boundary_obj(
    maior[[1]], sf::st_crs(g), fonte,
    if (nzchar(rotulo)) rotulo else as.character(fonte),
    paste(n[nzchar(n)], collapse = " "), aneis)
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
