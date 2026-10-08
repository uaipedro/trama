# Base multivariada: série múltipla, VAR e previsão.

canada <- function() {
  e <- new.env(); utils::data("Canada", package = "vars", envir = e)
  e$Canada
}

test_that("join alinha no período comum, nomeia e recusa frequência diferente", {
  a <- stats::ts(1:10, start = c(2000, 1), frequency = 4)
  b <- stats::ts(11:22, start = c(2000, 3), frequency = 4)
  j <- tr_series_join(list(a, b), nomes = "a, b")
  expect_equal(colnames(j), c("a", "b"))
  expect_equal(stats::start(j), c(2000, 3))
  expect_equal(nrow(j), 8L)
  expect_match(attr(j, "nota"), "4 observação")
  expect_equal(colnames(tr_series_join(list(a, b))), c("serie_1", "serie_2"))
  expect_error(tr_series_join(list(a, stats::ts(1:10, frequency = 12))), class = "tr_series_error_frequency_mismatch")
  expect_error(tr_series_join(list(a, b), nomes = "so_um"), class = "tr_series_error_bad_option")
  expect_error(tr_series_join(list(a)), class = "tr_series_error_too_short")
})

test_that("pick devolve a coluna como série univariada e recusa nome desconhecido", {
  j <- canada()
  p <- tr_series_pick(j, "prod")
  expect_null(dim(p))
  expect_equal(as.numeric(p), as.numeric(j[, "prod"]))
  expect_equal(stats::tsp(p), stats::tsp(j))
  expect_error(tr_series_pick(j, "xyz"), class = "tr_series_error_bad_option")
})

test_that("tabela -> séries -> tabela volta igual", {
  j <- canada()
  tab <- .tr_series_mts_tabela(j)
  m <- tr_series_from_table_mts(tab, valores = c("e", "prod", "rw", "U"), tempo = "tempo", frequencia = 4L)
  expect_equal(unclass(m), unclass(j), ignore_attr = TRUE)
  expect_equal(stats::tsp(m), stats::tsp(j))
  expect_error(tr_series_from_table_mts(tab, valores = "e"), class = "tr_series_error_blank_param")
})

test_that("o tipo series/mts recusa série univariada e nomes repetidos", {
  expect_error(.tr_series_guard_mts(datasets::Nile), class = "tr_series_error_not_multivariate")
  x <- canada()[, 1:2]; colnames(x) <- c("a", "a")
  expect_error(.tr_series_guard_mts(x), class = "tr_series_error_not_multivariate")
})

test_that("VAR = vars::VAR (p fixo e por critério), a 1e-12", {
  # Oráculo: o próprio `vars` (Pfaff 2008, doi:10.18637/jss.v027.i04).
  j <- canada()
  v <- tr_series_var(j, defasagens = 2L, deterministico = "ambos")
  ref <- vars::VAR(j, p = 2, type = "both")
  for (eq in colnames(j)) {
    expect_equal(stats::coef(v$ajuste$varresult[[eq]]), stats::coef(ref$varresult[[eq]]), tolerance = 1e-12)
  }
  auto <- tr_series_var(j, max_defasagens = 8L, criterio = "HQ", deterministico = "ambos")
  expect_equal(unname(auto$ajuste$p), unname(vars::VARselect(j, lag.max = 8, type = "both")$selection[["HQ(n)"]]))
  expect_match(auto$nota, "HQ")
  cf <- .tr_series_var_coefs(v)
  expect_equal(nrow(cf), 4L * (4L * 2L + 2L))
})

test_that("previsão do VAR = predict() do vars a 80 e 95%, a 1e-12", {
  j <- canada()
  v <- tr_series_var(j, defasagens = 2L)
  f <- tr_series_forecast(var = v, horizonte = 5L)
  expect_s3_class(f, "mforecast")
  p95 <- stats::predict(v$ajuste, n.ahead = 5, ci = 0.95)$fcst
  p80 <- stats::predict(v$ajuste, n.ahead = 5, ci = 0.80)$fcst
  for (nm in colnames(j)) {
    fc <- f$forecast[[nm]]
    expect_equal(as.numeric(fc$mean), unname(p95[[nm]][, "fcst"]), tolerance = 1e-12)
    expect_equal(as.numeric(fc$upper[, "95%"]), unname(p95[[nm]][, "upper"]), tolerance = 1e-12)
    expect_equal(as.numeric(fc$lower[, "80%"]), unname(p80[[nm]][, "lower"]), tolerance = 1e-12)
  }
  # Calendário: começa no período seguinte ao fim da série.
  expect_equal(stats::start(f$forecast$e$mean), c(2001, 1))
  tab <- .tr_series_forecast_tabela(f)
  expect_equal(nrow(tab), 4L * 5L)
  expect_equal(unique(tab$serie), colnames(j))
  expect_s3_class(tr_series_plot_forecast(f), "ggplot")
})

test_that("previsão do VAR recusa bootstrap e duas entradas", {
  v <- tr_series_var(canada(), defasagens = 1L)
  expect_error(tr_series_forecast(var = v, intervalo = "bootstrap"), class = "tr_series_error_bad_option")
  expect_error(tr_series_forecast(modelo = forecast::ets(datasets::Nile), var = v),
               class = "tr_series_error_bad_option")
})

test_that("acurácia do VAR: uma linha por série, no treino e contra as reais", {
  j <- canada()
  treino <- stats::window(j, end = c(1998, 4))
  teste <- stats::window(j, start = c(1999, 1))
  f <- tr_series_forecast(var = tr_series_var(treino, defasagens = 2L), horizonte = 8L)
  a <- tr_series_accuracy(f, reais = teste)
  expect_equal(sort(unique(a$serie)), sort(colnames(j)))
  expect_setequal(a$conjunto, c("treino", "teste"))
  # Linha de teste de uma série = acurácia univariada da mesma previsão.
  um <- tr_series_accuracy(f$forecast$prod, .tr_series_uni(teste[, "prod"]))
  expect_equal(a$rmse[a$serie == "prod"], um$rmse, tolerance = 1e-12)
  expect_error(tr_series_accuracy(f$forecast$prod, reais = teste), class = "tr_series_error_not_a_series")
})

test_that("os nós multivariados rodam no grafo", {
  reg <- series_registry()
  fl <- trama::tr_flow(reg) |>
    trama::tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
    trama::tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
    trama::tr_add("j", "series/join", nomes = "dax, cac", from = c("a", "b")) |>
    trama::tr_add("v", "series/var", defasagens = 1L, from = "j") |>
    trama::tr_add("p", "series/forecast", horizonte = 3L, from = "v")
  s <- trama::tr_store(tempfile())
  out <- trama::tr_value(trama::tr_flow_doc(fl), "p", registry = fl$registry, store = s)
  expect_s3_class(out, "mforecast")
  tab <- trama::tr_value(trama::tr_flow_doc(fl), "j", registry = fl$registry, store = s)
  expect_equal(colnames(tab), c("dax", "cac"))
})
