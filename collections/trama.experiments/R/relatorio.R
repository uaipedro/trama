# O que o plano mostra no relatório Quarto exportado (`report` do tipo): o
# delineamento, os termos simulados, os
# avisos e as primeiras unidades. O Markdown é o da `trama.data`.

#' O plano de experimento no relatório exportado.
#'
#' Resumo do delineamento (estrutura, unidades, semente, resposta), os termos
#' declarados por `experiments/effect`, os avisos e as primeiras unidades
#' com a alocação sorteada. É o `report` do tipo `experiments/plan`.
#'
#' @param x um plano (`experiments/plan`).
#' @return objeto `trama.data::tr_data_report`.
#' @export
tr_experiments_report_plan <- function(x) {
  res <- list(estrutura = x$estrutura, unidades = nrow(x$unidades), semente = x$semente,
              resposta = if (is.null(x$resposta)) NA_character_
                         else sprintf("%s (%s)", x$resposta$nome, x$resposta$distribuicao),
              analise = x$analise$no %||% NA_character_)
  termos <- if (length(x$termos)) {
    df <- data.frame(
      termo = vapply(x$termos, function(t) t$nome %||% "", ""),
      tipo = vapply(x$termos, function(t) t$tipo %||% "", ""),
      fator = vapply(x$termos, function(t) paste(t$fator, collapse = ":"), ""),
      stringsAsFactors = FALSE)
    c("", "**Termos simulados**", "", trama.data::tr_data_md_table(df, c(termo = "Termo", tipo = "Tipo", fator = "Fator")))
  }
  avisos <- if (length(x$avisos)) c("", paste0("> ", unlist(x$avisos)))
  vis <- x$unidades[, !startsWith(names(x$unidades), ".ef_"), drop = FALSE]
  trama.data::tr_data_report(c(
    trama.data::tr_data_md_summary(res, x$rotulo), termos, avisos, "",
    unclass(trama.data::tr_data_report_table(vis, max_linhas = 12L))))
}
