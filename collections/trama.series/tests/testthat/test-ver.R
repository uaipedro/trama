test_that("cada gráfico devolve um ggplot com a dimensão da view pendurada", {
  x <- serie_mensal()
  f <- tr_series_forecast(tr_series_ets(x), 12L)
  ps <- list(
    tr_series_plot(x, pontos = TRUE), tr_series_acf(x), tr_series_pacf(x),
    tr_series_lag_plot(x), tr_series_seasonal_plot(x), tr_series_subseries(x),
    tr_series_plot_decomposition(tr_series_stl(x)), tr_series_plot_forecast(f, 36L)
  )
  for (p in ps) {
    expect_s3_class(p, "ggplot")
    expect_length(attr(p, "tr_view_dim"), 2L)
    expect_gte(length(p$layers), 1L)
    # Desenhar de verdade: um erro de estética só aparece aqui, não no `+`.
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("correlograma: eixo em defasagens, sem a zero, e as de fora marcadas", {
  p <- tr_series_acf(serie_mensal(), 36L)
  d <- p$data
  expect_equal(d$defasagem, 1:36)
  expect_true(d$fora[d$defasagem == 12])
  expect_equal(tr_series_pacf(serie_mensal(), 24L)$data$defasagem, 1:24)
  # Automático: nunca menos de três ciclos numa série sazonal.
  expect_equal(max(tr_series_acf(serie_mensal())$data$defasagem), 36)
  expect_equal(max(tr_series_acf(serie_anual())$data$defasagem), 20)
})

test_that("defasagens: um painel por k, 12 por padrão na mensal", {
  p <- tr_series_lag_plot(serie_mensal())
  expect_equal(nlevels(p$data$painel), 12L)
  expect_equal(nlevels(tr_series_lag_plot(serie_anual())$data$painel), 4L)
  expect_error(tr_series_lag_plot(serie_mensal(), 17L), class = "tr_series_error_bad_option")
})

test_that("gráficos sazonais recusam série sem ciclo, e subséries com ciclo grande", {
  expect_error(tr_series_seasonal_plot(serie_anual()), class = "tr_series_error_no_season")
  expect_error(tr_series_subseries(serie_anual()), class = "tr_series_error_no_season")
  semanal <- stats::ts(stats::rnorm(160), frequency = 52)
  expect_error(tr_series_subseries(semanal), class = "tr_series_error_bad_frequency")
})

test_that("previsão: histórico corta só o passado mostrado", {
  f <- tr_series_forecast(tr_series_ets(serie_mensal()), 12L)
  p <- tr_series_plot_forecast(f, 24L)
  hist <- p$layers[[3]]$data
  expect_equal(nrow(hist), 24L)
  expect_equal(nrow(tr_series_plot_forecast(f)$layers[[3]]$data), 144L)
})

test_that("previews dos tipos gráficos são PNG pela view", {
  skip_if_not_installed("png")
  d <- tr_series_stl(serie_mensal())
  art <- series_decomposition_type()$preview(d, ctx_tmp())
  expect_equal(art$renderer, "trama/image")
  dims <- dim(png::readPNG(art$files$png))
  expect_equal(c(dims[[2]], dims[[1]]), c(1600, 1200))
  f <- tr_series_forecast(tr_series_ets(serie_mensal()), 12L)
  expect_true(file.exists(series_forecast_type()$preview(f, ctx_tmp())$files$png))
})

test_that("série no tempo com sobreposta: duas linhas no mesmo eixo, com legenda", {
  x <- serie_mensal()
  tend <- tr_series_component(tr_series_regression(x, grau = 1L), "tendencia")
  p <- tr_series_plot(x, sobreposta = tend)
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$colour)), 2L)
  expect_equal(levels(p$data$linha), c("original", "tendência"))
  expect_equal(p$data$valor[p$data$linha == "tendência"], as.numeric(tend))
  # Sem nome de componente, "estimada"; digitados, vencem; iguais, erro.
  expect_equal(levels(tr_series_plot(x, sobreposta = x + 1)$data$linha), c("original", "estimada"))
  expect_equal(levels(tr_series_plot(x, sobreposta = tend, nome_serie = "pax",
                                     nome_sobreposta = "reta")$data$linha), c("pax", "reta"))
  expect_error(tr_series_plot(x, sobreposta = tend, nome_serie = "tendência"),
               class = "tr_series_error_bad_option")
  # Sem sobreposta, o desenho de sempre: uma linha, sem mapeamento de cor.
  expect_null(tr_series_plot(x)$mapping$colour)
})

test_that("sobreposta: frequência diferente é erro; janela diferente dá o eixo da união", {
  x <- serie_mensal()
  expect_error(tr_series_plot(x, sobreposta = stats::aggregate(x, nfrequency = 1)),
               class = "tr_series_error_frequency_mismatch")
  expect_error(tr_series_plot(x, sobreposta = 1:3), class = "tr_series_error_not_a_series")
  # 1955 a 1965: começa depois e termina depois da original (1949–1960).
  curta <- stats::ts(c(as.numeric(stats::window(x, start = c(1955, 1))), 1:60),
                     start = c(1955, 1), frequency = 12)
  p <- tr_series_plot(x, sobreposta = curta)
  faixa <- range(p$data$tempo)
  expect_equal(faixa, as.Date(c("1949-01-01", "1965-12-01")))
})

test_that("pelo motor: série em 'serie' e tendência em 'sobreposta'", {
  reg <- series_registry(); s <- trama::tr_store(tempfile())
  f <- trama::tr_flow(reg) |>
    trama::tr_add("ap", "series/example", dataset = "AirPassengers") |>
    trama::tr_add("reg", "series/regression", grau = 1L, from = "ap") |>
    trama::tr_add("tend", "series/component", componente = "tendencia", from = "reg") |>
    trama::tr_add("g", "series/plot", from = c("ap", "tend"))
  p <- trama::tr_value(f$doc, "g", registry = reg, store = s)
  # O nome do componente atravessa o store da série.
  expect_equal(levels(p$data$linha), c("original", "tendência"))
})
