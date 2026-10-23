# O que os tipos daqui mostram no relatório Quarto exportado (`report` do
# tipo): o resumo dos pontos, a tabela do variograma, os parâmetros do modelo
# e o MAPA da superfície — que é o que se lê de uma krigagem. O Markdown é o
# da `trama.data`.

.tr_spatial_md <- function(...) trama.data::tr_data_report(c(...))

#' O que os tipos da `trama.spatial` mostram no relatório exportado.
#'
#' `report` de cada tipo: `spatial/points` (variável, n, quartis, projeção),
#' `spatial/variogram` (a tabela por classe de distância), `spatial/model` (os
#' parâmetros ajustados) e `spatial/surface` (o mapa do predito).
#'
#' @param x o objeto do tipo.
#' @return objeto `trama.data::tr_data_report` (a superfície devolve o mapa,
#'   um ggplot).
#' @export
tr_spatial_report_points <- function(x) {
  z <- x$dados[[x$variavel]]
  q <- stats::quantile(z, c(0, .25, .5, .75, 1), names = FALSE, na.rm = TRUE)
  res <- list(variavel = x$variavel, unidade = x$unidade, n = nrow(x$dados),
              coordenadas = paste(x$coord_cols, collapse = ", "),
              projecao = if (inherits(x$crs, "crs")) x$crs$input else NA_character_,
              minimo = q[[1]], q1 = q[[2]], mediana = q[[3]], q3 = q[[4]], maximo = q[[5]],
              borda = !is.null(x$borda))
  .tr_spatial_md(trama.data::tr_data_md_summary(res, x$rotulo))
}

#' @rdname tr_spatial_report_points
#' @export
tr_spatial_report_boundary <- function(x) {
  res <- list(fonte = x$fonte, area = x$area, vertices = x$n_vertices,
              aneis_descartados = x$n_aneis_descartados,
              projecao = if (inherits(x$crs, "crs")) x$crs$input else NA_character_)
  .tr_spatial_md(trama.data::tr_data_md_summary(res, x$rotulo))
}

#' @rdname tr_spatial_report_points
#' @export
tr_spatial_report_variogram <- function(x) {
  t <- as.data.frame(x$tabela)[intersect(c("u", "gamma", "np"), names(x$tabela))]
  .tr_spatial_md(
    sprintf("**Variograma de %s** (%s%s)", x$variavel, x$estimador,
            if (!is.null(x$tendencia) && nzchar(x$tendencia)) sprintf(", tendência: %s", x$tendencia) else ""),
    "", trama.data::tr_data_md_table(t, c(u = "Distância", gamma = "γ(h)", np = "Pares")),
    if (nzchar(x$nota %||% "")) c("", sprintf("*%s*", x$nota)))
}

#' @rdname tr_spatial_report_points
#' @export
tr_spatial_report_anisotropy <- function(x) {
  t <- as.data.frame(x$tabela)[, c("direcao", "u", "gamma", "np")]
  .tr_spatial_md(
    sprintf("**Variogramas direcionais de %s** (%s, direções %s, tolerância %g graus)",
            x$variavel, x$estimador, paste(x$direcoes, collapse = ", "),
            x$tolerancia),
    "",
    "A leitura é visual: alcances diferentes por direção sugerem anisotropia. O bloco não faz teste de hipótese.",
    "", trama.data::tr_data_md_table(t, c(direcao = "Direção", u = "Distância",
                                          gamma = "γ(h)", np = "Pares")),
    if (nzchar(x$nota %||% "")) c("", sprintf("*%s*", x$nota)))
}

#' @rdname tr_spatial_report_points
#' @export
tr_spatial_report_model <- function(x) {
  res <- list(familia = x$familia, metodo = x$metodo, pepita = x$pepita, contribuicao = x$contribuicao,
              patamar = x$patamar, alcance = x$alcance, alcance_pratico = x$alcance_pratico,
              kappa = if (is.null(x$kappa)) NA else x$kappa,
              grau_de_dependencia = x$grau_dependencia, sqr = x$sqr)
  .tr_spatial_md(trama.data::tr_data_md_summary(res, "Modelo de variograma"),
                 if (nzchar(x$nota %||% "")) c("", sprintf("*%s*", x$nota)))
}

#' @rdname tr_spatial_report_points
#' @export
tr_spatial_report_validation <- function(x) {
  res <- list(metodo = x$metodo, dobras = x$dobras, n = nrow(x$tabela),
              me = x$metricas$me, rmse = x$metricas$rmse,
              msdr = x$metricas$msdr, correlacao = x$metricas$correlacao)
  .tr_spatial_md(
    trama.data::tr_data_md_summary(res, sprintf("Validação cruzada de %s",
                                                x$pontos$variavel)),
    "",
    "O MSDR é a média de (resíduo / erro-padrão)²: perto de 1 o erro-padrão do mapa está calibrado.",
    if (nzchar(x$nota %||% "")) c("", sprintf("*%s*", x$nota)))
}

#' @rdname tr_spatial_report_points
#' @export
tr_spatial_report_surface <- function(x) tr_spatial_map(x)
