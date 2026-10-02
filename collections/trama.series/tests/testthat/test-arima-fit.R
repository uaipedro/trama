# ARIMA nos blocos de modelo. Oráculos: `lmtest::lrtest()` para a razão de
# verossimilhança e o `aicc` do próprio `forecast::Arima()` (Hyndman e
# Athanasopoulos, 2021, sec. 9.8); tolerância 1e-8.

lh <- datasets::lh  # 48 hormônios luteinizantes, o exemplo de AR do `stats`

test_that("compare: AR(1) contra AR(2) é a razão de verossimilhança do lrtest", {
  skip_if_not_installed("lmtest")
  m1 <- tr_series_arima(lh, automatico = FALSE, p = 1L, d = 0L, q = 0L)
  m2 <- tr_series_arima(lh, automatico = FALSE, p = 2L, d = 0L, q = 0L)
  r <- trama.models::tr_models_compare(tr_series_as_fit(m2), tr_series_as_fit(m1))
  o <- lmtest::lrtest(m1, m2)
  expect_equal(r$estatistica, o$Chisq[[2]], tolerance = 1e-8)
  expect_equal(r$p_valor, o$`Pr(>Chisq)`[[2]], tolerance = 1e-8)
  expect_equal(as.numeric(r$gl), o$Df[[2]])
  expect_match(r$h0, "ar2", fixed = TRUE)
})

test_that("compare recusa d diferente, série diferente e ordens não aninhadas", {
  m1 <- tr_series_arima(lh, automatico = FALSE, p = 1L, d = 0L, q = 0L)
  expect_error(trama.models::tr_models_compare(
    tr_series_as_fit(m1), tr_series_as_fit(tr_series_arima(lh, automatico = FALSE, p = 1L, d = 1L, q = 0L))),
    class = "tr_series_error_not_nested")
  expect_error(trama.models::tr_models_compare(
    tr_series_as_fit(m1), tr_series_as_fit(tr_series_arima(lh * 2, automatico = FALSE, p = 2L, d = 0L, q = 0L))),
    class = "tr_series_error_not_nested")
  expect_error(trama.models::tr_models_compare(
    tr_series_as_fit(m1), tr_series_as_fit(tr_series_arima(lh, automatico = FALSE, p = 0L, d = 0L, q = 1L))))
})

test_that("select: o AICc do ARIMA é o do forecast", {
  ms <- lapply(0:2, function(p) tr_series_arima(lh, automatico = FALSE, p = p, d = 0L, q = 1L))
  r <- trama.models::tr_models_select(lapply(ms, tr_series_as_fit), criterio = "AICc")
  esperado <- vapply(ms, function(m) m$aicc, numeric(1))
  expect_equal(r$AICc, sort(esperado), tolerance = 1e-8)
  rb <- trama.models::tr_models_select(lapply(ms, tr_series_as_fit), criterio = "BIC")
  expect_equal(rb$BIC, sort(vapply(ms, function(m) m$bic, numeric(1))), tolerance = 1e-8)
})

test_that("coeficientes: estimativa e erro-padrão do coeftest (z)", {
  skip_if_not_installed("lmtest")
  m <- tr_series_arima(lh, automatico = FALSE, p = 2L, d = 0L, q = 0L)
  tab <- trama.models::tr_models_coefficients(tr_series_as_fit(m))$tabela
  o <- lmtest::coeftest(m)
  expect_equal(tab$estimativa, unname(o[, 1]), tolerance = 1e-8)
  expect_equal(tab$erro_padrao, unname(o[, 2]), tolerance = 1e-8)
  expect_equal(tab$p_valor, unname(o[, 4]), tolerance = 1e-8)
})

test_that("adaptador recusa ETS e liga ARIMA ao compare pelo fio", {
  expect_error(tr_series_as_fit(tr_series_ets(datasets::AirPassengers, "ANN")),
               class = "tr_series_error_not_arima")
  reg <- series_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("s", "series/example") |>
    trama::tr_add("a1", "series/arima", automatico = FALSE, p = 1L, d = 0L, q = 0L, from = "s") |>
    trama::tr_add("a2", "series/arima", automatico = FALSE, p = 2L, d = 0L, q = 0L, from = "s") |>
    trama::tr_add("cmp", "models/compare", from = "a1") |>
    trama::tr_link("a2", "cmp:outro") |>
    trama::tr_add("sel", "models/select", from = c("a1", "a2"))
  rodar <- function(flow, no) trama::tr_value(trama::tr_flow_doc(flow), no, registry = flow$registry,
                                               store = trama::tr_store(tempfile()))
  expect_equal(rodar(f, "cmp")$teste, "Razão de verossimilhança")
  expect_equal(nrow(rodar(f, "sel")), 2L)
})
