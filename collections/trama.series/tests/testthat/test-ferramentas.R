# Atributo `trama_ferramentas`: cada ramo registra SÓ as ferramentas que correu,
# para o relatório citar o que de fato rodou. Dados: os exemplos da coleção.

canada_var <- function() {
  e <- new.env(); utils::data("Canada", package = "vars", envir = e)
  e$Canada
}

ferramentas <- function(x) attr(x, "trama_ferramentas", exact = TRUE)

test_that("regressão: lm no erro independente, gls com erro ARMA", {
  expect_equal(ferramentas(tr_series_regression(serie_mensal())), "stats::lm")
  arma <- tr_series_regression(serie_mensal(), erro = "arma", ar = 1L, ma = 0L)
  expect_equal(ferramentas(arma), c("nlme::gls", "nlme::corARMA"))
})

test_that("previsão de regressão: predict do lm, ou Arima + forecast com erro ARMA", {
  fit <- tr_series_regression(serie_mensal())
  expect_equal(ferramentas(tr_series_forecast(ajuste = fit, horizonte = 3L)), "stats::predict")
  fit_arma <- tr_series_regression(serie_mensal(), erro = "arma", ar = 1L, ma = 0L)
  expect_equal(ferramentas(tr_series_forecast(ajuste = fit_arma, horizonte = 3L)),
               c("forecast::Arima", "forecast::forecast"))
})

test_that("previsão de VAR registra o predict do vars; de modelo fica o declarado", {
  v <- tr_series_var(canada_var(), defasagens = 1L)
  expect_equal(ferramentas(tr_series_forecast(var = v, horizonte = 2L)), "vars::predict")
  expect_null(ferramentas(tr_series_forecast(tr_series_arima(log(serie_mensal())), 6L)))
})

test_that("ARIMA automático e manual", {
  x <- log(serie_mensal())
  expect_equal(ferramentas(tr_series_arima(x)), "forecast::auto.arima")
  manual <- tr_series_arima(x, automatico = FALSE, p = 0L, d = 1L, q = 1L)
  expect_equal(ferramentas(manual), "forecast::Arima")
})

test_that("previsão de referência: cada método, a sua função do forecast", {
  x <- serie_mensal()
  expect_equal(ferramentas(tr_series_baseline(x, "média")), "forecast::meanf")
  expect_equal(ferramentas(tr_series_baseline(x, "ingênuo")), "forecast::naive")
  expect_equal(ferramentas(tr_series_baseline(x, "ingênuo sazonal")), "forecast::snaive")
  expect_equal(ferramentas(tr_series_baseline(x, "deriva")), "forecast::rwf")
})

test_that("diferenças: ndiffs e, com ciclo, nsdiffs", {
  expect_equal(ferramentas(tr_series_ndiffs(serie_mensal())), c("forecast::ndiffs", "forecast::nsdiffs"))
  expect_equal(ferramentas(tr_series_ndiffs(serie_anual())), "forecast::ndiffs")
})

test_that("série que sai é dado: transformar, média móvel e interpolar não registram (declaram)", {
  # Atributo em série vaza para quem faz conta com ela (log, escala, lag).
  x <- serie_mensal()
  expect_null(ferramentas(tr_series_transform(x, "boxcox")))
  expect_null(ferramentas(tr_series_moving_average(x, 12L)))
  com_falta <- x; com_falta[c(5, 20)] <- NA
  expect_null(ferramentas(tr_series_interpolate(com_falta)))
})

test_that("Phillips-Perron: stats com tendência, urca na constante", {
  expect_equal(ferramentas(tr_series_phillips_perron(serie_mensal())), "stats::PP.test")
  expect_equal(ferramentas(tr_series_phillips_perron(serie_mensal(), "constante")), "urca::punitroot")
})

test_that("Granger: vars::causality no Wald; VAR refeito e ndiffs no Toda-Yamamoto", {
  aj <- tr_series_var(canada_var(), defasagens = 2L, deterministico = "constante")
  expect_equal(ferramentas(tr_series_granger(aj, causa = "e")), "vars::causality")
  expect_equal(ferramentas(tr_series_granger(aj, causa = "e", metodo = "toda_yamamoto")),
               c("vars::VAR", "forecast::ndiffs"))
})

test_that("VECM registra o ca.jo, o cajorls e o vec2var", {
  out <- tr_series_vecm(canada_var(), posto = 1L, defasagens = 2L)
  expect_equal(ferramentas(out), c("urca::ca.jo", "urca::cajorls", "vars::vec2var"))
})
