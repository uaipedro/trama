# models/jarque_bera (R/testes.R).
#
# Oráculos: `tseries::jarque.bera.test` (Trapletti & Hornik) e
# `moments::jarque.test` (Komsta & Novomestky), os dois com os momentos de
# divisor n de Jarque & Bera (1987). Tolerância 1e-12 (relativa) na
# estatística: mesma conta, só a ordem das somas muda. No p-valor, 1e-12
# ABSOLUTA: os dois oráculos fazem `1 - pchisq()`, que perde dígitos
# relativos quando p é pequeno (faithful: 5,7e-8, diferença de 1,6e-17); o
# bloco usa a cauda superior direto, que é a mais precisa.

test_that("JB de mtcars$mpg e de faithful$eruptions = tseries e moments (1e-12)", {
  skip_if_not_installed("tseries")
  for (caso in list(list(d = datasets::mtcars, v = "mpg"), list(d = datasets::faithful, v = "eruptions"))) {
    out <- tr_models_jarque_bera(caso$d, caso$v)
    ref <- tseries::jarque.bera.test(caso$d[[caso$v]])
    expect_equal(out$estatistica, unname(ref$statistic), tolerance = 1e-12, info = caso$v)
    expect_lt(abs(out$p_valor - unname(ref$p.value)), 1e-12, label = caso$v)
    expect_equal(as.numeric(out$gl), 2)
  }
  skip_if_not_installed("moments")
  x <- datasets::faithful$eruptions
  out <- tr_models_jarque_bera(datasets::faithful, "eruptions")
  ref <- moments::jarque.test(x)
  expect_equal(out$estatistica, unname(ref$statistic), tolerance = 1e-12)
  expect_lt(abs(out$p_valor - unname(ref$p.value)), 1e-12)
  expect_equal(out$extra$assimetria, moments::skewness(x), tolerance = 1e-12)
  expect_equal(out$extra$curtose_excesso, moments::kurtosis(x) - 3, tolerance = 1e-12)
})

test_that("faltantes saem da conta e a nota avisa n pequeno", {
  d <- data.frame(y = c(datasets::mtcars$mpg, NA))
  out <- tr_models_jarque_bera(d, "y")
  expect_equal(out$extra$n, 32L)
  expect_match(out$nota, "models/shapiro", fixed = TRUE)
  expect_s3_class(out, "tr_models_test")
  grande <- tr_models_jarque_bera(datasets::faithful, "eruptions")
  expect_false(grepl("models/shapiro", grande$nota, fixed = TRUE))
})

test_that("coluna constante, não numérica ou com menos de 3 valores é erro classificado", {
  expect_error(tr_models_jarque_bera(data.frame(y = rep(2, 10)), "y"), class = "tr_models_error_fit")
  expect_error(tr_models_jarque_bera(datasets::iris, "Species"))
  expect_error(tr_models_jarque_bera(data.frame(y = c(1, 2)), "y"), class = "tr_models_error_too_few_rows")
})
