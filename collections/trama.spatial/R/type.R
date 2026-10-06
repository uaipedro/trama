# Os tipos da coleção, e os adaptadores que os ligam à `data`.
#
# - `spatial/points`: a variável regionalizada COM as coordenadas, a projeção,
#   a unidade e a borda. É a ideia central da coleção, como a amostra que
#   carrega o desenho é a da `sampling`.
#
# - `spatial/variogram`: o variograma empírico, com a tabela por classe e os
#   parâmetros que o geraram.
#
# O preço de um tipo próprio seria perder a `data` e a `view`, e são os
# ADAPTADORES que o pagam: o motor os insere na aresta, sem caixa na tela.
#
# Todo `store` é FUNIL, na doutrina das irmãs.

#' Molde dos tipos da coleção: RDS, tema do projeto, store que confere antes de gravar.
#' @noRd
.tr_spatial_rds_type <- function(id, label, color, conferir, preview, summary = NULL) {
  trama::tr_type(
    id, version = 1L, label = label, color = color, ext = "rds", tema = TRUE,
    store = function(x, path) { conferir(x); saveRDS(x, path, compress = FALSE) },
    restore = function(path) readRDS(path),
    summary = summary, preview = preview)
}

# ---- spatial/points ------------------------------------------------------------

.tr_spatial_pontos_conferir <- function(x) {
  .tr_spatial_guard(x, "tr_spatial_points", .TR_SPATIAL_CAMPOS_PONTOS,
                    "tr_spatial_error_not_points", "um objeto de pontos")
}

.tr_spatial_pontos_preview <- function(x) {
  z <- x$dados[[x$variavel]]
  q <- stats::quantile(z, c(0, .25, .5, .75, 1), names = FALSE)
  c(list(rotulo = x$rotulo, variavel = x$variavel, n = nrow(x$dados),
         unidade = .tr_spatial_nulo(x$unidade),
         crs = if (inherits(x$crs, "crs")) x$crs$input else NULL,
         quartis = as.list(signif(q, 6)),
         tem_borda = !is.null(x$borda),
         pontos = lapply(seq_len(nrow(x$coords)), function(i) {
           list(x = x$coords[i, 1], y = x$coords[i, 2], z = z[[i]])
         }),
         borda = if (is.null(x$borda)) NULL else lapply(seq_len(nrow(x$borda)), function(i) {
           list(x = x$borda[i, 1], y = x$borda[i, 2])
         }),
         nota = x$nota),
    .tr_spatial_linhas_json(x$dados))
}

spatial_points_type <- function() {
  .tr_spatial_rds_type("spatial/points", "Pontos", "#0e7490", .tr_spatial_pontos_conferir,
                       function(x, ctx) trama::tr_preview("spatial/points",
                                                          data = .tr_spatial_pontos_preview(x)))
}

#' Pontos -> tabela: a tabela original, que já traz as coordenadas.
#' @noRd
.tr_spatial_pontos_tabela <- function(x) x$dados

# ---- spatial/variogram ---------------------------------------------------------

.TR_SPATIAL_CAMPOS_VARIO <- c("tabela", "estimador", "dist_max", "n_classes", "direcao",
                              "tolerancia", "tendencia", "variavel", "unidade", "pontos", "nota")

.tr_spatial_vario_conferir <- function(x) {
  .tr_spatial_guard(x, "tr_spatial_variogram", .TR_SPATIAL_CAMPOS_VARIO,
                    "tr_spatial_error_not_a_variogram", "um variograma")
  if (!all(c("u", "gamma", "np") %in% names(x$tabela))) {
    .tr_spatial_abort("tr_spatial_error_not_a_variogram",
      "O variograma precisa das colunas 'u', 'gamma' e 'np'.")
  }
  invisible(x)
}

.tr_spatial_vario_preview <- function(x) {
  t <- x$tabela
  c(list(variavel = x$variavel, estimador = x$estimador, tendencia = x$tendencia,
         unidade = .tr_spatial_nulo(x$unidade), dist_max = x$dist_max,
         direcao = .tr_spatial_nulo(x$direcao), tolerancia = .tr_spatial_nulo(x$tolerancia),
         classes = lapply(seq_len(nrow(t)), function(i) {
           list(u = t$u[[i]], gamma = t$gamma[[i]], np = t$np[[i]])
         }),
         nota = x$nota),
    .tr_spatial_linhas_json(t))
}

spatial_variogram_type <- function() {
  .tr_spatial_rds_type("spatial/variogram", "Variograma", "#6366f1", .tr_spatial_vario_conferir,
                       function(x, ctx) trama::tr_preview("spatial/variogram",
                                                          data = .tr_spatial_vario_preview(x)))
}

#' Variograma -> tabela: uma linha por classe de distância.
#' @noRd
.tr_spatial_vario_tabela <- function(x) x$tabela

.tr_spatial_adapters <- function() {
  list(trama::tr_adapter("spatial/points", "data/table", .tr_spatial_pontos_tabela),
       trama::tr_adapter("spatial/variogram", "data/table", .tr_spatial_vario_tabela))
}
