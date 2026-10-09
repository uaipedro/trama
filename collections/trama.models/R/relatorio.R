# O que os tipos daqui mostram no relatório Quarto exportado (`report` do
# tipo). Mesma ideia do card: o quadro com cabeçalho de livro, o rodapé (CV,
# média, n) e a fonte — não a lista crua. O Markdown é o da `trama.data`.

#' Cabeçalhos do quadro como no card: "FV" e "Pr > F" no quadro da ANOVA,
#' "termo" e "Pr > |t|" nos coeficientes.
#' @noRd
.tr_models_rotulos_md <- function(ef) {
  t <- ef$tabela
  cab <- ifelse(names(t) %in% names(.TR_MODELS_CABECALHOS), .TR_MODELS_CABECALHOS[names(t)], names(t))
  names(cab) <- names(t)
  if ("termo" %in% names(t) && !grepl("ANOVA", ef$titulo)) cab[["termo"]] <- "Termo"
  cab[["p_valor"]] <- switch(ef$coluna_estat %||% "F", t = "Pr > |t|", z = "Pr > |z|",
                             qui2 = "Pr > χ²", "Pr > F")
  cab
}

.tr_models_md_efeitos <- function(ef, titulo = TRUE) {
  t <- as.data.frame(ef$tabela)
  # Colunas que não se aplicam a este modelo (VIF com um preditor, p. ex.)
  # vêm inteiras em NA: no card não aparecem, aqui também não.
  t <- t[, !vapply(t, function(v) all(is.na(v)), logical(1)) | names(t) %in% c("termo", "p_valor"), drop = FALSE]
  ef$tabela <- t
  rod <- ef$rodape
  rodape <- if (length(rod)) paste(sprintf("%s = %s", names(rod), unlist(rod)), collapse = " · ")
  c(if (titulo) c(sprintf("**%s**", ef$titulo), ""),
    trama.data::tr_data_md_table(t, .tr_models_rotulos_md(ef)),
    if (length(rodape) || nzchar(ef$nota %||% "")) c("", paste0("*", .tr_models_nota(rodape, ef$nota), "*")),
    if (nzchar(ef$fonte %||% "")) c("", sprintf("Fonte: %s.", ef$fonte)))
}

#' Um quadro de efeitos no relatório exportado.
#'
#' O quadro da ANOVA, dos coeficientes ou das comparações como tabela, com
#' título, rodapé (CV, média, n) e fonte. É o `report` do tipo
#' `models/effects`.
#'
#' @param x um quadro de efeitos (`models/effects`).
#' @return objeto `trama.data::tr_data_report`.
#' @export
tr_models_report_effects <- function(x) trama.data::tr_data_report(.tr_models_md_efeitos(x))

#' Um modelo ajustado no relatório exportado.
#'
#' O resumo que o card mostra: a fórmula, as medidas de ajuste que existem para
#' o modelo (R², CV, AIC…), o teste global quando há um, e o quadro de efeitos —
#' a ANOVA nos delineamentos, os coeficientes no resto. É o `report` do tipo
#' `models/fit`.
#'
#' @param x um modelo (`models/fit`).
#' @return objeto `trama.data::tr_data_report`.
#' @export
tr_models_report_fit <- function(x) {
  est <- tr_models_fit_stats(x)
  num <- function(v) trama.data::tr_data_fmt_num(v)
  medidas <- c("R²" = est$r2, "R² aj." = est$r2_ajustado, "R² marg." = est$r2_marginal,
               "R² cond." = est$r2_condicional, "Desvio expl." = est$desvio_explicado,
               "σ" = est$sigma, "CV (%)" = est$cv_pct, "AIC" = est$aic)
  medidas <- medidas[!is.na(medidas)]
  global <- tryCatch(.tr_models_teste_global(x), error = function(e) NULL)
  ef <- tryCatch(.tr_models_efeitos_do_fit(x), error = function(e) NULL)
  nota <- .tr_models_nota_fit(x)
  trama.data::tr_data_report(c(
    sprintf("**%s** · `%s` · n = %d", x$rotulo, if (is.character(x$formula)) x$formula else paste(deparse(x$formula), collapse = " "), est$n),
    "",
    if (length(medidas)) c(paste(sprintf("%s = %s", names(medidas), num(medidas)), collapse = " · "), ""),
    if (!is.null(global)) c(sprintf("%s: p %s", global$rotulo,
                                    sub("^([0-9])", "= \\1", trama.data::tr_data_fmt_p(global$p))), ""),
    if (!is.null(ef)) .tr_models_md_efeitos(ef),
    if (nzchar(nota)) c("", sprintf("*%s.*", sub("\\.$", "", nota)))))
}

#' Médias ajustadas no relatório exportado.
#'
#' A tabela das médias com erro-padrão, intervalo e as letras, e o ajuste com
#' que as letras foram feitas. É o `report` do tipo `models/emm`.
#'
#' @param x médias ajustadas (`models/emm`).
#' @return objeto `trama.data::tr_data_report`.
#' @export
tr_models_report_emm <- function(x) {
  rot <- c(media = "Média", erro_padrao = "EP", gl = "GL", li = "LI", ls = "LS", grupo = "Grupo")
  trama.data::tr_data_report(c(
    sprintf("**Médias ajustadas de %s**%s", x$resposta,
            if (length(x$por)) sprintf(" (por %s)", paste(x$por, collapse = ", ")) else ""),
    "",
    trama.data::tr_data_md_table(x$tabela, rot),
    "",
    sprintf("*Médias seguidas da mesma letra não diferem (%s, α = %s).*", x$ajuste,
            trama.data::tr_data_fmt_num(x$alfa))))
}
