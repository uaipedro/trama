# Intervalos de previsão por bootstrap dos resíduos (Hyndman & Athanasopoulos,
# FPP3, sec. 5.5): oráculo é o próprio forecast::forecast(bootstrap = TRUE) com a
# mesma semente e o mesmo RNG, igual à máquina.
com_semente <- function(s, expr) { RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(s); expr }

test_that("bootstrap reproduz forecast::forecast(bootstrap = TRUE) com a mesma semente (ARIMA e ETS)", {
  x <- log(datasets::AirPassengers)
  for (m in list(forecast::Arima(x, order = c(0, 1, 1), seasonal = c(0, 1, 1)),
                 forecast::ets(x, model = "AAA"))) {
    t <- tr_series_forecast(m, horizonte = 12L, intervalo = "bootstrap", .seed = 42L)
    ref <- com_semente(42L, forecast::forecast(m, h = 12, level = c(80, 95),
                                                bootstrap = TRUE, npaths = 5000))
    expect_equal(unclass(t$lower), unclass(ref$lower), tolerance = 1e-12)
    expect_equal(unclass(t$upper), unclass(ref$upper), tolerance = 1e-12)
    expect_equal(t$mean, ref$mean, tolerance = 1e-12)
  }
})

test_that("mesma semente, mesmo leque; o padrão continua normal", {
  m <- forecast::ets(datasets::Nile)
  a <- tr_series_forecast(m, 10L, "bootstrap", .seed = 7L)
  b <- tr_series_forecast(m, 10L, "bootstrap", .seed = 7L)
  expect_identical(a$upper, b$upper)
  expect_equal(tr_series_forecast(m, 10L)$upper,
               forecast::forecast(m, h = 10, level = c(80, 95))$upper)
})

test_that("Holt-Winters não tem bootstrap: recusa com classe", {
  hw <- stats::HoltWinters(datasets::co2)
  expect_error(tr_series_forecast(hw, 12L, "bootstrap", .seed = 1L), class = "tr_series_error_bad_option")
  expect_error(tr_series_forecast(forecast::ets(datasets::Nile), 5L, "boot"),
               class = "tr_series_error_bad_option")
})
