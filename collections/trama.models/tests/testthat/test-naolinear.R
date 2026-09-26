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

# Oráculo publicado: NIST StRD, regressão não linear (valores certificados,
# https://www.itl.nist.gov/div898/strd/nls/nls_main.shtml). Rat42 é a logística
# b1 / (1 + exp(b2 − b3·x)) = SSlogis com Asym = b1, xmid = b2/b3, scal = 1/b3;
# Misra1d é b1·b2·x/(1 + b2·x) = Michaelis-Menten com Vm = b1, K = 1/b2.
test_that("NIST StRD: Rat42 (logístico) e Misra1d (Michaelis-Menten) batem com os certificados", {
  rat42 <- data.frame(y = c(8.93, 10.8, 18.59, 22.33, 39.35, 56.11, 61.73, 64.62, 67.08),
                      x = c(9, 14, 21, 28, 42, 57, 63, 70, 79))
  m <- tr_models_nls(rat42, "y", "x", "logístico")
  cf <- stats::coef(m$ajuste)
  b <- c(7.2462237576E+01, 2.6180768402E+00, 6.7359200066E-02)
  expect_equal(unname(cf), c(b[1], b[2] / b[3], 1 / b[3]), tolerance = 1e-6)
  expect_equal(sum(stats::residuals(m$ajuste)^2), 8.0565229338, tolerance = 1e-7)
  # EP de Asym = EP certificado de b1 (a mesma coordenada).
  expect_equal(tr_models_coefficients(m)$tabela$erro_padrao[[1]], 1.7340283401, tolerance = 1e-5)

  misra <- data.frame(
    y = c(10.07, 14.73, 17.94, 23.93, 29.61, 35.18, 40.02, 44.82, 50.76, 55.05, 61.01, 66.40, 75.47, 81.78),
    x = c(77.6, 114.9, 141.1, 190.8, 239.9, 289.0, 332.8, 378.4, 434.8, 477.3, 536.8, 593.1, 689.1, 760.0))
  mm <- tr_models_nls(misra, "y", "x", "Michaelis-Menten")
  cm <- stats::coef(mm$ajuste)
  expect_equal(unname(cm), c(4.3736970754E+02, 1 / 3.0227324449E-04), tolerance = 1e-6)
  expect_equal(sum(stats::residuals(mm$ajuste)^2), 5.6419295283E-02, tolerance = 1e-6)
  expect_equal(tr_models_coefficients(mm)$tabela$erro_padrao[[1]], 3.6489174345, tolerance = 1e-5)
})
