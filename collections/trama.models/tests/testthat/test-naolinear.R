# Não linear: cada modelo recupera os parâmetros com que os dados foram
# simulados, e a não convergência é um erro com classe que diz o que fazer.

simular <- function(f, x, sd, seed) {
  set.seed(seed)
  data.frame(x = x, y = f(x) + stats::rnorm(length(x), sd = sd))
}

test_that("os cinco modelos recuperam os parâmetros simulados", {
  x <- rep(seq(0, 20, 1), 3)
  casos <- list(
    list(m = "logístico", f = function(x) 30 / (1 + exp((8 - x) / 2.5)), p = c(Asym = 30, xmid = 8, scal = 2.5)),
    list(m = "Michaelis-Menten", f = function(x) 50 * x / (4 + x), p = c(Vm = 50, K = 4)),
    list(m = "exponencial assintótico", f = function(x) 40 + (5 - 40) * exp(-exp(log(0.2)) * x),
         p = c(Asym = 40, R0 = 5, lrc = log(0.2))),
    list(m = "Gompertz", f = function(x) 25 * exp(-6 * 0.7^x), p = c(Asym = 25, b2 = 6, b3 = 0.7)),
    list(m = "linear-platô", f = function(x) 2 + 0.5 * pmin(x, 12), p = c(a = 2, b = 0.5, x0 = 12)))
  for (k in seq_along(casos)) {
    cs <- casos[[k]]
    m <- tr_models_nls(simular(cs$f, x, 0.3, k), "y", "x", cs$m)
    est <- stats::coef(m$ajuste)[names(cs$p)]
    expect_equal(unname(est), unname(cs$p), tolerance = 0.05, info = cs$m)
    # O IC de Wald cobre o valor simulado.
    co <- tr_models_coefficients(m)$tabela
    expect_true(all(co$li_95 <= cs$p[co$termo] & co$ls_95 >= cs$p[co$termo]), info = cs$m)
  }
})

test_that("contrato: previsão, medidas, resíduos e o card", {
  d <- simular(function(x) 30 / (1 + exp((8 - x) / 2.5)), rep(0:20, 2), 1, 11)
  m <- tr_models_nls(d, "y", "x", "logístico")
  expect_s3_class(m, "tr_models_nls")
  expect_equal(tr_models_info(m)$preditores, "x")
  p <- tr_models_predict(m, data.frame(x = 8))
  expect_equal(p$previsto, as.numeric(stats::predict(m$ajuste, data.frame(x = 8))))
  s <- tr_models_fit_stats(m)
  expect_true(is.na(s$r2))
  expect_equal(s$r2_pseudo, 1 - sum(stats::residuals(m$ajuste)^2) / sum((d$y - mean(d$y))^2))
  expect_equal(s$rmse, sqrt(mean(stats::residuals(m$ajuste)^2)))
  expect_equal(s$aic, stats::AIC(m$ajuste))
  expect_equal(nrow(tr_models_residuals(m)), nrow(d))
  cv <- tr_models_predict(m, validacao = "cruzada")
  expect_false(anyNA(cv$previsto))
  expect_s3_class(tr_models_shapiro_residuals(m), "tr_models_test")
  expect_error(tr_models_breusch_pagan(m), class = "tr_models_error_not_applicable")
  expect_error(tr_models_anova_table(m), class = "tr_models_error_not_applicable")
  expect_error(tr_models_coefficients(m, escala = "desvio padrão"), class = "tr_models_error_not_applicable")
  expect_s3_class(tr_models_plot_regression(m), "ggplot")
  # O RDS guarda e devolve o modelo inteiro.
  f <- tempfile(fileext = ".rds"); saveRDS(m, f)
  expect_equal(tr_models_predict(readRDS(f), data.frame(x = 8))$previsto, p$previsto)
})

test_that("não convergência sai com classe e diz a forma esperada", {
  ruido <- data.frame(x = 1:10, y = c(5, 1, 8, 2, 9, 3, 7, 1, 6, 2))
  e <- tryCatch(tr_models_nls(ruido, "y", "x", "logístico"), error = identity)
  expect_s3_class(e, "tr_models_error_no_convergence")
  expect_match(conditionMessage(e), "não convergiu", fixed = TRUE)
  expect_match(conditionMessage(e), "um S que sobe", fixed = TRUE)
  expect_error(tr_models_nls(data.frame(x = c(1, 1, 2, 2, 3), y = 1:5), "y", "x", "logístico"),
               class = "tr_models_error_too_few_rows")
})

test_that("Michaelis-Menten no Puromycin bate com o nls direto", {
  pu <- ex("Puromycin")
  m <- tr_models_nls(pu, "rate", "conc", "Michaelis-Menten")
  ref <- stats::nls(rate ~ SSmicmen(conc, Vm, K), data = datasets::Puromycin)
  expect_equal(stats::coef(m$ajuste), stats::coef(ref))
})
