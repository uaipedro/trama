# Modelo de intervenção (Box & Tiao 1975, JASA 70:70-79), forma de ordem zero:
# ARIMA com um regressor de degrau, pulso ou rampa. Oráculo: forecast::Arima
# com o mesmo xreg, coeficientes e erros-padrão a 1e-8.
sb <- function() log(datasets::Seatbelts[, "drivers"])

test_that("degrau em 1983-02 no Seatbelts: o regressor é a coluna law, e o ajuste é o do forecast::Arima", {
  x <- sb()
  t <- tr_series_intervencao(x, data = "1983, 2", tipo = "degrau",
                             p = 1L, d = 0L, q = 0L, P = 1L, D = 1L, Q = 1L, constante = FALSE)
  law <- as.numeric(datasets::Seatbelts[, "law"])
  ref <- forecast::Arima(x, order = c(1, 0, 0), seasonal = c(1, 1, 1), xreg = cbind(intervencao = law),
                         include.constant = FALSE)
  ef <- t[t$termo == "intervencao", ]
  expect_equal(ef$estimativa, unname(stats::coef(ref)[["intervencao"]]), tolerance = 1e-8)
  expect_equal(ef$erro_padrao, unname(sqrt(diag(ref$var.coef))[["intervencao"]]), tolerance = 1e-8)
  expect_equal(ef$li_95, ef$estimativa - stats::qnorm(0.975) * ef$erro_padrao, tolerance = 1e-12)
  expect_equal(ef$p_valor, 2 * stats::pnorm(-abs(ef$estimativa / ef$erro_padrao)), tolerance = 1e-12)
  expect_equal(nrow(t), length(stats::coef(ref)))
  expect_equal(ef$efeito_pct, 100 * (exp(ef$estimativa) - 1), tolerance = 1e-12)
})

test_that("pulso e rampa montam o regressor certo", {
  x <- stats::ts(as.numeric(datasets::Nile))
  for (tp in c("pulso", "rampa")) {
    t <- tr_series_intervencao(x, data = "30", tipo = tp, p = 1L, d = 0L, q = 1L,
                               P = 0L, D = 0L, Q = 0L, constante = TRUE)
    reg <- if (tp == "pulso") as.numeric(seq_along(x) == 30) else pmax(0, seq_along(x) - 29)
    ref <- forecast::Arima(x, order = c(1, 0, 1), xreg = cbind(intervencao = reg), include.constant = TRUE)
    o <- match(names(stats::coef(ref)), t$termo)
    expect_equal(t$estimativa[o], unname(stats::coef(ref)), tolerance = 1e-8)
    expect_equal(t$erro_padrao[o], unname(sqrt(diag(ref$var.coef))), tolerance = 1e-8)
    expect_equal(t$termo[[1]], "intervencao")
  }
})

test_that("bordas: data fora da série, na primeira observação, faltante, tipo inválido", {
  x <- sb()
  expect_error(tr_series_intervencao(x, data = "1990, 1"), class = "tr_series_error_bad_period")
  expect_error(tr_series_intervencao(x, data = "1969, 1"), class = "tr_series_error_bad_period")
  expect_error(tr_series_intervencao(x, data = "1983, 2", tipo = "salto"), class = "tr_series_error_bad_option")
  expect_error(tr_series_intervencao(datasets::presidents, data = "1960, 1"),
               class = "tr_series_error_missing_values")
})
