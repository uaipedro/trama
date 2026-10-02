# O ARIMA como `models/fit`.
#
# O ajuste continua saindo como `series/model` (o tipo que previsão, resíduos e
# acurácia leem), e este adaptador o leva ao contrato da `trama.models` quando
# o fio vai para um leitor de modelo: `models/compare` (razão de
# verossimilhança entre ordens aninhadas), `models/select` (AICc),
# `models/coefficients`, `models/fit_stats`. Comparar e selecionar continuam
# sendo blocos da `models` — um verbo, um bloco —; o que a série fornece é a
# verossimilhança (`tr_models_loglik`) e a regra de aninhamento
# (`tr_models_nesting`).
#
# Só o ARIMA entra: ETS e Holt-Winters também têm verossimilhança, mas o
# aninhamento entre eles não é uma relação entre conjuntos de coeficientes, e o
# adaptador recusa dizendo isso em vez de fingir.

#' Adaptador `series/model` → `models/fit`, só para ARIMA.
#'
#' @param x um ajuste de `series/arima` (`forecast::Arima`/`auto.arima`).
#' @return um `tr_models_fit` de classe `tr_series_arima_fit`.
#' @export
tr_series_as_fit <- function(x) {
  if (!inherits(x, "Arima")) {
    .tr_series_abort("tr_series_error_not_arima",
                     paste0("Só o ARIMA entra nos blocos de modelo (comparar, selecionar, coeficientes); ",
                            "chegou um '%s'. Para comparar ETS e Holt-Winters, use 'series/accuracy'."),
                     class(x)[[1]])
  }
  dados <- tibble::tibble(valor = as.numeric(x$x))
  structure(list(ajuste = x, classe = "arima", rotulo = .tr_series_arima_rotulo(x),
                 formula = "valor ~ 1", dados = dados, resposta = "valor", nota = "",
                 descartadas = sum(is.na(x$x))),
            class = c("tr_series_arima_fit", "tr_models_fit"))
}

#' "ARIMA(2,0,0) com média", como o `print` do `forecast` escreve.
#' @noRd
.tr_series_arima_rotulo <- function(x) {
  o <- forecast::arimaorder(x)
  r <- sprintf("ARIMA(%d,%d,%d)", o[["p"]], o[["d"]], o[["q"]])
  if (length(o) > 3L && any(o[c("P", "D", "Q")] > 0)) {
    r <- sprintf("%s(%d,%d,%d)[%d]", r, o[["P"]], o[["D"]], o[["Q"]], o[["Frequency"]])
  }
  cf <- names(stats::coef(x))
  if ("intercept" %in% cf) r <- paste(r, "com média")
  if ("drift" %in% cf) r <- paste(r, "com deriva")
  r
}

#' Os parâmetros estimados (fora os fixados) e quantos entram na
#' verossimilhança: os coeficientes livres mais a variância — a mesma conta do
#' `forecast` (`npar = length(coef[mask]) + 1`).
#' @noRd
.tr_series_arima_npar <- function(aj) length(stats::coef(aj)[aj$mask]) + 1L

#' Verossimilhança no contrato: `n` é o número de observações efetivas
#' (`nobs`: a série menos as perdidas nas diferenças), o que dá o AICc do
#' `forecast` (Hyndman e Athanasopoulos, 2021, sec. 9.8) pela fórmula do
#' `models/select`.
#' @export
tr_models_loglik.tr_series_arima_fit <- function(x) {
  aj <- x$ajuste
  list(ll = as.numeric(aj$loglik), k = .tr_series_arima_npar(aj), n = as.numeric(aj$nobs))
}

#' Aninhamento entre ARIMA: mesma série, mesmas diferenças (d, D e período) e
#' os coeficientes do menor todos no maior. É o que deixa a razão de
#' verossimilhança valer: com d diferente as verossimilhanças são de séries
#' diferentes (a diferenciada e a original).
#' @export
tr_models_nesting.tr_series_arima_fit <- function(x, outro) {
  no <- "models/compare"
  a <- x$ajuste; b <- outro$ajuste
  if (!identical(as.numeric(a$x), as.numeric(b$x)) || !identical(stats::tsp(a$x), stats::tsp(b$x))) {
    .tr_series_abort("tr_series_error_not_nested",
                     "'%s': os dois ARIMA foram ajustados em séries diferentes.", no)
  }
  oa <- forecast::arimaorder(a); ob <- forecast::arimaorder(b)
  dif <- function(o) c(o[["d"]], if (length(o) > 3L) c(o[["D"]], o[["Frequency"]]) else c(0, 1))
  if (!identical(unname(dif(oa)), unname(dif(ob)))) {
    .tr_series_abort("tr_series_error_not_nested",
                     paste0("'%s': %s e %s diferenciam a série de jeitos diferentes (d ou D); as ",
                            "verossimilhanças são de séries diferentes e não se comparam. Fixe as ",
                            "mesmas diferenças nos dois."), no, x$rotulo, outro$rotulo)
  }
  ca <- names(stats::coef(a)); cb <- names(stats::coef(b))
  if (!all(ca %in% cb) || length(cb) == length(ca)) {
    .tr_series_abort("tr_series_error_not_nested",
                     paste0("'%s': %s não está aninhado em %s — os termos do menor (%s) têm de estar ",
                            "todos no maior (%s), e o maior ter algum a mais."),
                     no, x$rotulo, outro$rotulo, paste(ca, collapse = ", "), paste(cb, collapse = ", "))
  }
  paste(setdiff(cb, ca), collapse = ", ")
}

#' @export
tr_models_info.tr_series_arima_fit <- function(x) {
  list(tarefa = "regressao", resposta = "valor", preditores = character(), niveis = NULL,
       n = as.numeric(x$ajuste$nobs), rotulo = x$rotulo, familia = "gaussian")
}

#' Coeficientes com erro-padrão da matriz de informação e teste z (Wald):
#' o que `lmtest::coeftest()` dá para um `Arima`.
#' @export
tr_models_coefs.tr_series_arima_fit <- function(x, exponenciar = FALSE, escala = "unidade",
                                                confianca = 0.95, ...) {
  if (isTRUE(exponenciar) || !identical(escala, "unidade")) {
    .tr_series_abort("tr_series_error_bad_option",
                     "'models/coefficients': num ARIMA não há exponenciar nem escala por desvio padrão.")
  }
  aj <- x$ajuste
  est <- stats::coef(aj)[aj$mask]
  ep <- sqrt(diag(aj$var.coef))
  z <- est / ep
  q <- stats::qnorm(1 - (1 - confianca) / 2)
  pct <- sub(".", "_", as.character(round(100 * confianca, 1)), fixed = TRUE)
  tab <- tibble::tibble(termo = names(est), estimativa = unname(est), erro_padrao = unname(ep),
                        z = unname(z), p_valor = 2 * stats::pnorm(-abs(unname(z))),
                        li = unname(est - q * ep), ls = unname(est + q * ep))
  names(tab)[names(tab) == "li"] <- paste0("li_", pct)
  names(tab)[names(tab) == "ls"] <- paste0("ls_", pct)
  trama.models::tr_models_effects(tab, "Coeficientes", coluna_estat = "z",
                                  rodape = list(n = as.character(aj$nobs)),
                                  nota = sprintf("%s; erro-padrão da matriz de informação, teste z (Wald)", x$rotulo))
}

#' @export
tr_models_stats.tr_series_arima_fit <- function(x) {
  aj <- x$ajuste
  tibble::tibble(modelo = x$rotulo, formula = x$formula, n = as.numeric(aj$nobs),
                 sigma = sqrt(aj$sigma2), aic = aj$aic, aicc = aj$aicc, bic = aj$bic,
                 log_verossimilhanca = as.numeric(aj$loglik))
}

#' @export
tr_models_resid.tr_series_arima_fit <- function(x) {
  aj <- x$ajuste
  r <- as.numeric(stats::residuals(aj))
  tibble::tibble(valor = as.numeric(aj$x), ajustado = as.numeric(stats::fitted(aj)), residuo = r,
                 residuo_padronizado = r / sqrt(aj$sigma2))
}

#' @export
tr_models_predict_raw.tr_series_arima_fit <- function(x, novos, ...) {
  .tr_series_abort("tr_series_error_bad_option",
                   "'models/predict' não prevê ARIMA: a previsão é no tempo, com 'series/forecast'.")
}

#' @export
tr_models_predict_cv.tr_series_arima_fit <- function(x, validacao = "resubstituição") {
  .tr_series_abort("tr_series_error_bad_option",
                   "Avaliar um ARIMA é com 'series/accuracy' (previsão um passo à frente, fora da amostra).")
}

#' @export
tr_models_importance.tr_series_arima_fit <- function(x) {
  .tr_series_abort("tr_series_error_bad_option",
                   "'models/importance' não se aplica a ARIMA: leia os coeficientes.")
}

#' @export
tr_models_card.tr_series_arima_fit <- function(x, ctx) {
  trama::tr_preview("trama/text", data = list(text = paste(utils::capture.output(print(x$ajuste)), collapse = "\n")))
}

#' @export
tr_models_as_table.tr_series_arima_fit <- function(x) tr_models_coefs.tr_series_arima_fit(x)$tabela
