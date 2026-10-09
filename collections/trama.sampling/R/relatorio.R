# O que os tipos daqui mostram no relatório Quarto exportado (`report` do
# tipo): o tamanho e a alocação no plano, o desenho na amostra, a tabela das
# estimativas com o erro do desenho, o resumo das réplicas na simulação. O
# Markdown é o da `trama.data`.

.tr_sampling_md <- function(...) trama.data::tr_data_report(c(...))
.tr_sampling_md_nota <- function(nota) if (length(nota) && nzchar(nota)) c("", sprintf("*%s*", nota))

#' O que os tipos da `trama.sampling` mostram no relatório exportado.
#'
#' `report` de cada tipo: `sampling/plan` (tamanho, erro, confiança e a
#' alocação), `sampling/sample` (o desenho e as primeiras linhas),
#' `sampling/estimate` (estimativa, erro-padrão, intervalo, CV e deff) e
#' `sampling/simulation` (o resumo das réplicas contra o valor verdadeiro).
#'
#' @param x o objeto do tipo.
#' @return objeto `trama.data::tr_data_report`.
#' @export
tr_sampling_report_plan <- function(x) {
  res <- list(tipo = x$tipo, n = x$n, confianca = x$confianca, erro = x$erro,
              erro_alcancado = x$erro_alcancado, conglomerados = x$conglomerados,
              tamanho_conglomerado = x$tamanho_conglomerado)
  .tr_sampling_md(
    trama.data::tr_data_md_summary(res, x$rotulo),
    if (!is.null(x$alocacao)) c("", "**Alocação**", "", trama.data::tr_data_md_table(x$alocacao)),
    .tr_sampling_md_nota(x$nota))
}

#' @rdname tr_sampling_report_plan
#' @export
tr_sampling_report_sample <- function(x) {
  w <- x$dados$peso_amostral
  res <- list(metodo = x$metodo, n = x$n, N = x$N, estratos = x$estrato_col,
              conglomerados = x$conglomerado_col, peso_minimo = min(w), peso_maximo = max(w),
              soma_dos_pesos = sum(w))
  .tr_sampling_md(trama.data::tr_data_md_summary(res, x$rotulo), "",
                  unclass(trama.data::tr_data_report_table(x$dados, max_linhas = 10L)),
                  .tr_sampling_md_nota(x$nota))
}

#' @rdname tr_sampling_report_plan
#' @export
tr_sampling_report_estimate <- function(x) {
  t <- as.data.frame(x$tabela)
  t <- cbind(data.frame(linha = .tr_sampling_rotulos_linhas(x), stringsAsFactors = FALSE),
             t[intersect(c("estimativa", "erro_padrao", "li", "ls", "cv_pct", "deff", "n"), names(t))])
  t <- t[, !vapply(t, function(v) all(is.na(v)), logical(1)), drop = FALSE]
  rot <- c(linha = "", estimativa = "Estimativa", erro_padrao = "EP",
           li = sprintf("LI %s%%", trama.data::tr_data_fmt_num(100 * x$confianca)),
           ls = sprintf("LS %s%%", trama.data::tr_data_fmt_num(100 * x$confianca)),
           cv_pct = "CV (%)", deff = "deff", n = "n")
  .tr_sampling_md(sprintf("**%s · %s** (desenho: %s)", x$quantidade, x$variavel, x$desenho), "",
                  trama.data::tr_data_md_table(t, rot), .tr_sampling_md_nota(x$nota))
}

#' @rdname tr_sampling_report_plan
#' @export
tr_sampling_report_simulation <- function(x) {
  .tr_sampling_md(sprintf("**%s** · %d réplicas", x$rotulo, nrow(x$replicas)), "",
                  unclass(trama.data::tr_data_report_table(x$resumo)))
}
