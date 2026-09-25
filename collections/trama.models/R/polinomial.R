# Regressão polinomial nos níveis quantitativos de um tratamento da ANOVA.
#
# A SQ de tratamentos (k - 1 gl) se decompõe em graus sucessivos: linear,
# quadrático, cúbico..., cada um com 1 gl, e o que sobra até k - 1 é a falta de
# ajuste. Cada parcela é o acréscimo de SQ ao somar x^j ao modelo com os graus
# menores (e o bloco) — os polinômios ortogonais no caso balanceado e
# igualmente espaçado, e a mesma decomposição sequencial com espaçamento ou
# repetições desiguais. Todos os F usam o QM e os gl do resíduo da ANOVA.

.TR_MODELS_GRAUS <- c("linear", "quadrático", "cúbico", "quártico", "quíntico")

#' Regressão polinomial nos tratamentos quantitativos (polinômios ortogonais).
#' @param modelo objeto `tr_models_fit` de `models/anova_dic` ou `models/anova_dbc`.
#' @param tratamento o fator de níveis numéricos (dose, época, espaçamento).
#' @param grau maior grau testado (1 a 5, e menor que o número de níveis).
#' @param alfa nível para escolher o grau da equação.
#' @return objeto `tr_models_effects`, com `coeficientes`, `grau_equacao` e `r2`.
#' @export
tr_models_polinomial <- function(modelo, tratamento = "", grau = 3L, alfa = 0.05) {
  .tr_models_fit_conferir(modelo)
  no <- "models/polinomial"
  .tr_models_exigir(modelo, "lm", no, "A regressão nos tratamentos parte de uma ANOVA ('models/anova_dic' ou 'models/anova_dbc').")
  if (!identical(modelo$delineamento, "DIC") && !identical(modelo$delineamento, "DBC")) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "'%s': só para DIC e DBC de um fator (o modelo é %s).", no, modelo$rotulo)
  }
  grau <- as.integer(.tr_models_num(grau, "grau", min = 1, max = 5))
  alfa <- .tr_models_num(alfa, "alfa", min = 0.001, max = 0.5)
  trat <- .tr_models_col(modelo$dados, tratamento, "tratamento")
  if (!trat %in% modelo$tratamentos) {
    .tr_models_abort("tr_models_error_not_applicable", "'%s': '%s' não é o tratamento do modelo.", no, trat)
  }
  d <- modelo$dados
  x <- suppressWarnings(as.numeric(as.character(d[[trat]])))
  if (anyNA(x)) {
    .tr_models_abort("tr_models_error_not_applicable",
                     "'%s': os níveis de '%s' não são números (a regressão pede tratamento quantitativo).", no, trat)
  }
  k <- length(unique(x))
  if (grau > k - 1L) {
    .tr_models_abort("tr_models_error_bad_option",
                     "'%s': com %d níveis o grau vai até %d (pediu %d).", no, k, k - 1L, grau)
  }
  y <- d[[modelo$resposta]]
  ctrl <- if (identical(modelo$delineamento, "DBC")) modelo$bloco else character()
  P <- stats::poly(x, grau)
  base <- data.frame(y = y, .fx = factor(x), d[, ctrl, drop = FALSE])
  for (j in seq_len(grau)) base[[paste0(".p", j)]] <- P[, j]
  f <- stats::reformulate(c(ctrl, paste0(".p", seq_len(grau)), ".fx"), "y")
  a <- .tr_models_ajustar(stats::anova(stats::lm(f, data = base)), no)
  q <- tr_models_anova_table(modelo)$tabela
  qmr <- q$qm[q$termo == "Resíduo"]; glr <- q$gl[q$termo == "Resíduo"]
  linhas <- paste0(".p", seq_len(grau))
  sq <- c(a[linhas, "Sum Sq"], if (k - 1L > grau) a[".fx", "Sum Sq"])
  gl <- c(rep(1, grau), if (k - 1L > grau) k - 1L - grau)
  termo <- c(.TR_MODELS_GRAUS[seq_len(grau)], if (k - 1L > grau) "falta de ajuste")
  fv <- (sq / gl) / qmr
  tab <- data.frame(termo = c(termo, "Resíduo"), gl = c(gl, glr), sq = c(sq, qmr * glr),
                    qm = c(sq / gl, qmr), F = c(fv, NA), p_valor = c(stats::pf(fv, gl, glr, lower.tail = FALSE), NA))
  sig <- which(tab$p_valor[seq_len(grau)] < alfa)
  ge <- if (length(sig)) max(sig) else 0L
  rodape <- list()
  cf <- numeric(); r2 <- NA_real_
  if (ge > 0L) {
    medias <- tapply(y, x, mean); r <- as.vector(table(x)); xs <- as.numeric(names(medias))
    cf <- unname(stats::coef(stats::lm(medias ~ stats::poly(xs, ge, raw = TRUE), weights = r)))
    r2 <- sum(sq[seq_len(ge)]) / sum(sq)
    termos <- vapply(seq_along(cf), function(j) {
      v <- .tr_models_fmt(abs(cf[[j]]), 4L)
      s <- if (j == 1L) (if (cf[[j]] < 0) "-" else "") else if (cf[[j]] < 0) " - " else " + "
      paste0(s, v, if (j == 2L) "x" else if (j > 2L) paste0("x^", j - 1L) else "")
    }, "")
    rodape[["equação"]] <- paste0("ŷ = ", paste(termos, collapse = ""))
    rodape[["R²"]] <- .tr_models_pct(100 * r2)
  }
  falta <- if (k - 1L > grau) tab$p_valor[[grau + 1L]] else NA
  nota <- .tr_models_nota(
    sprintf("%d níveis de %s; F com o QM do resíduo da ANOVA (%d gl)", k, trat, as.integer(glr)),
    if (ge == 0L) sprintf("nenhum grau significativo a %s%%: sem equação", formatC(100 * alfa, format = "fg", decimal.mark = ","))
    else sprintf("equação de grau %d, o maior significativo a %s%%", ge, formatC(100 * alfa, format = "fg", decimal.mark = ",")),
    if (!is.na(falta) && falta < alfa) "falta de ajuste significativa: o polinômio não descreve bem as médias" else "",
    if (length(unique(table(x))) > 1L) "repetições desiguais: decomposição sequencial" else "")
  ef <- .tr_models_efeitos(tab, sprintf("Regressão polinomial em %s", trat), coluna_estat = "F",
                           rodape = rodape, nota = nota, fonte = "Pimentel-Gomes (2009); Banzatto & Kronka (2006)")
  ef$coeficientes <- cf; ef$grau_equacao <- ge; ef$r2 <- r2
  ef
}
