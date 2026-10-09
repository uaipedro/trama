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
.tr_spatial_rds_type <- function(id, label, color, conferir, preview, summary = NULL, report = NULL) {
  trama::tr_type(
    id, version = 1L, label = label, color = color, ext = "rds", tema = TRUE,
    store = function(x, path) { conferir(x); saveRDS(x, path, compress = FALSE) },
    restore = function(path) readRDS(path),
    summary = summary, preview = preview, report = report)
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
         coord_cols = as.list(x$coord_cols),
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
                                                          data = .tr_spatial_pontos_preview(x)),
                       report = tr_spatial_report_points)
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
                                                          data = .tr_spatial_vario_preview(x)),
                       report = tr_spatial_report_variogram)
}

#' Variograma -> tabela: uma linha por classe de distância.
#' @noRd
.tr_spatial_vario_tabela <- function(x) x$tabela

# ---- spatial/model -------------------------------------------------------------

.TR_SPATIAL_CAMPOS_MODELO <- c("familia", "pepita", "contribuicao", "alcance",
                               "alcance_pratico", "patamar", "kappa", "metodo", "sqr",
                               "grau_dependencia", "variograma", "nota")

.tr_spatial_modelo_conferir <- function(x) {
  .tr_spatial_guard(x, "tr_spatial_model", .TR_SPATIAL_CAMPOS_MODELO,
                    "tr_spatial_error_not_a_model", "um modelo ajustado")
}

spatial_model_type <- function() {
  .tr_spatial_rds_type("spatial/model", "Modelo de variograma", "#8b5cf6", .tr_spatial_modelo_conferir,
                       function(x, ctx) trama.view::tr_view_render(.tr_spatial_plot_modelo(x), ctx),
                       report = tr_spatial_report_model)
}

#' Modelo -> tabela: uma linha por parâmetro numérico, com família, método e
#' kappa repetidos em toda linha, para que `bind_rows` de dois ajustes (duas
#' famílias, dois métodos) continue distinguindo um do outro.
#'
#' `valor` é numérico: misturar texto aqui coagiria todos os números a texto,
#' com perda de precisão e `"NA"` no lugar de `NA`.
#' @noRd
.tr_spatial_modelo_tabela <- function(x) {
  tibble::tibble(
    parametro = c("pepita", "contribuicao", "patamar", "alcance", "alcance_pratico",
                  "grau_dependencia", "sqr"),
    valor = c(x$pepita, x$contribuicao, x$patamar, x$alcance, x$alcance_pratico,
              x$grau_dependencia, x$sqr),
    familia = x$familia, metodo = x$metodo, kappa = as.numeric(x$kappa))
}

# ---- spatial/surface -----------------------------------------------------------

.TR_SPATIAL_CAMPOS_SUPERFICIE <- c("grade", "tipo", "modelo", "pontos", "borda",
                                   "resolucao", "vizinhanca", "variavel", "unidade", "nota")

.tr_spatial_superficie_conferir <- function(x) {
  .tr_spatial_guard(x, "tr_spatial_surface", .TR_SPATIAL_CAMPOS_SUPERFICIE,
                    "tr_spatial_error_not_a_surface", "uma superficie predita")
}

spatial_surface_type <- function() {
  .tr_spatial_rds_type("spatial/surface", "Superfície predita", "#0d9488",
                       .tr_spatial_superficie_conferir,
                       function(x, ctx) trama.view::tr_view_render(tr_spatial_map(x), ctx),
                       report = tr_spatial_report_surface)
}

#' Superfície -> tabela: uma linha por célula da grade, com coordenadas, predito,
#' variância e erro-padrão.
#' @noRd
.tr_spatial_superficie_tabela <- function(x) x$grade

.tr_spatial_adapters <- function() {
  list(trama::tr_adapter("spatial/points", "data/table", .tr_spatial_pontos_tabela),
       trama::tr_adapter("spatial/boundary", "data/table", .tr_spatial_borda_tabela),
       trama::tr_adapter("spatial/anisotropy", "data/table", .tr_spatial_aniso_tabela),
       trama::tr_adapter("spatial/variogram", "data/table", .tr_spatial_vario_tabela),
       trama::tr_adapter("spatial/model", "data/table", .tr_spatial_modelo_tabela),
       trama::tr_adapter("spatial/surface", "data/table", .tr_spatial_superficie_tabela))
}
