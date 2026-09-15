test_that("ARIMA automático e manual, com o nome do modelo no resumo", {
  x <- log(serie_mensal())
  m <- tr_series_arima(x)
  expect_s3_class(m, "Arima")
  expect_match(.tr_series_metodo(m), "^ARIMA\\(")
  m2 <- tr_series_arima(x, automatico = FALSE, p = 0L, d = 1L, q = 1L, P = 0L, D = 1L, Q = 1L)
  expect_equal(.tr_series_metodo(m2), "ARIMA(0,1,1)(0,1,1)[12]")
  expect_error(tr_series_arima(serie_anual(), automatico = FALSE, D = 1L),
               class = "tr_series_error_no_season")
  expect_error(tr_series_arima(x, automatico = FALSE, p = 9L), class = "tr_series_error_bad_option")
})

test_that("falha de ajuste vira erro classificado, com a causa junto", {
  # Amortecer uma tendência que não existe (N) é combinação proibida no ETS:
  # a recusa é do `forecast`, e a classe e o nome do nó são nossos.
  err <- tryCatch(tr_series_ets(serie_mensal(), "ANN", amortecida = "sim"), error = identity)
  expect_s3_class(err, "tr_series_error_fit")
  expect_match(conditionMessage(err), "series/ets", fixed = TRUE)
  expect_match(conditionMessage(err$parent), "Forbidden", fixed = TRUE)
  expect_error(tr_series_ets(serie_mensal(), "AXA"), class = "tr_series_error_bad_ets")
  expect_error(tr_series_ets(serie_mensal(), ""), class = "tr_series_error_blank_param")
  expect_error(tr_series_ets(serie_anual(), "ANA"), class = "tr_series_error_no_season")
})

test_that("ETS e Holt-Winters ajustam e preveem", {
  e <- tr_series_ets(serie_mensal(), "MAM", amortecida = "não")
  expect_s3_class(e, "ets")
  expect_equal(.tr_series_metodo(e), "ETS(M,A,M)")
  # `auto` deixa o ajuste escolher o amortecimento — e nesta série ele escolhe.
  expect_equal(.tr_series_metodo(tr_series_ets(serie_mensal(), "MAM", amortecida = "sim")),
               "ETS(M,Ad,M)")
  hw <- tr_series_holt_winters(serie_mensal(), tipo = "multiplicativa")
  expect_s3_class(hw, "HoltWinters")
  expect_match(.tr_series_metodo(hw), "multiplicative")
  hw2 <- tr_series_holt_winters(serie_anual(), sazonalidade = FALSE)
  expect_equal(.tr_series_metodo(hw2), "Holt-Winters (tendência)")
  for (m in list(e, hw, hw2)) {
    f <- tr_series_forecast(m, 6L)
    expect_s3_class(f, "forecast")
    expect_length(f$mean, 6L)
  }
})

test_that("previsão: horizonte, tabela com os dois níveis, e tipo", {
  f <- tr_series_forecast(tr_series_arima(serie_mensal()), 24L)
  tab <- .tr_series_forecast_tabela(f)
  expect_equal(nrow(tab), 24L)
  expect_equal(tab$tempo[[1]], as.Date("1961-01-01"))
  expect_true(all(tab$li_95 <= tab$li_80 & tab$li_80 <= tab$previsto & tab$previsto <= tab$ls_80))
  s <- series_forecast_type()$summary(f)
  expect_equal(s$de, "1961 jan")
  expect_equal(s$ate, "1962 dez")
  expect_error(tr_series_forecast(tr_series_arima(serie_mensal()), 0L), class = "tr_series_error_bad_option")
  expect_error(series_forecast_type()$store(serie_mensal(), tempfile()),
               class = "tr_series_error_not_a_forecast")
})

test_that("referências: os quatro métodos, e o sazonal recusa série anual", {
  for (mt in c("média", "ingênuo", "ingênuo sazonal", "deriva")) {
    f <- tr_series_baseline(serie_mensal(), mt, 12L)
    expect_s3_class(f, "forecast")
  }
  sn <- tr_series_baseline(serie_mensal(), "ingênuo sazonal", 12L)
  expect_equal(as.numeric(sn$mean), as.numeric(serie_mensal()[133:144]))
  expect_error(tr_series_baseline(serie_anual(), "ingênuo sazonal"), class = "tr_series_error_no_season")
})

test_that("resíduos são série, e o tipo modelo recusa o que não é modelo", {
  m <- tr_series_arima(serie_mensal())
  r <- tr_series_residuals(m)
  expect_true(stats::is.ts(r))
  expect_null(dim(r))
  expect_error(series_model_type()$store(serie_mensal(), tempfile()), class = "tr_series_error_not_a_model")
  s <- series_model_type()$summary(m)
  expect_true(all(c("metodo", "aic", "observacoes", "sigma") %in% names(s)))
  art <- series_model_type()$preview(m, ctx_tmp())
  expect_equal(art$renderer, "trama/text")
  expect_match(art$data$text, "ARIMA")
})

test_that("acurácia: treino sozinho, treino e teste, e série real que não cobre", {
  x <- serie_mensal()
  treino <- tr_series_window(x, fim = "1958")
  f <- tr_series_forecast(tr_series_ets(treino), 24L)
  a1 <- tr_series_accuracy(f)
  expect_equal(a1$conjunto, "treino")
  expect_true(all(c("rmse", "mae", "mape", "mase") %in% names(a1)))
  a2 <- tr_series_accuracy(f, x)
  expect_equal(a2$conjunto, c("treino", "teste"))
  expect_error(tr_series_accuracy(f, treino), class = "tr_series_error_no_overlap")
  expect_error(tr_series_accuracy(f, serie_anual()), class = "tr_series_error_no_overlap")
})
