test_that("decomposição clássica e STL saem na mesma forma, e somam a série", {
  x <- serie_mensal()
  for (d in list(tr_series_decompose(x), tr_series_stl(x), tr_series_stl(x, 13L, robusta = TRUE))) {
    expect_s3_class(d, "tr_series_decomp")
    soma <- d$tendencia + d$sazonal + d$resto
    ok <- !is.na(soma)
    expect_equal(as.numeric(soma[ok]), as.numeric(x[ok]), tolerance = 1e-8)
  }
})

test_that("multiplicativa multiplica, e dessazonalizada divide", {
  x <- serie_mensal()
  d <- tr_series_decompose(x, "multiplicativa")
  prod <- d$tendencia * d$sazonal * d$resto
  ok <- !is.na(prod)
  expect_equal(as.numeric(prod[ok]), as.numeric(x[ok]), tolerance = 1e-8)
  expect_equal(tr_series_component(d, "dessazonalizada"), x / d$sazonal)
  expect_equal(tr_series_component(tr_series_stl(x), "dessazonalizada"),
               x - tr_series_stl(x)$sazonal)
  expect_identical(tr_series_component(d, "tendencia"), d$tendencia)
})

test_that("decompor recusa série anual, curta, com faltante ou não positiva", {
  expect_error(tr_series_decompose(serie_anual()), class = "tr_series_error_no_season")
  expect_error(tr_series_stl(stats::ts(1:20, frequency = 12)), class = "tr_series_error_too_short")
  expect_error(tr_series_stl(datasets::presidents), class = "tr_series_error_missing_values")
  z <- serie_mensal(); z[[5]] <- 0
  expect_error(tr_series_decompose(z, "multiplicativa"), class = "tr_series_error_nonpositive")
  expect_error(tr_series_stl(serie_mensal(), 5L), class = "tr_series_error_bad_option")
})

test_that("decomposição: tipo, resumo com a força, e adaptador para tabela", {
  ty <- series_decomposition_type()
  d <- tr_series_stl(serie_mensal())
  s <- ty$summary(d)
  expect_equal(s$metodo, "STL")
  expect_gt(s$forca_sazonal, 0.5)
  tab <- .tr_series_decomp_tabela(d)
  expect_equal(names(tab), c("tempo", "observado", "tendencia", "sazonal", "resto"))
  expect_equal(tab$tempo[[1]], as.Date("1949-01-01"))
  expect_error(ty$store(serie_mensal(), tempfile()), class = "tr_series_error_not_a_decomposition")
})

test_that("rótulos de estação seguem a frequência", {
  expect_equal(.tr_series_estacoes(12L)[[1]], "jan")
  expect_equal(.tr_series_estacoes(12L)[[12]], "dez")
  expect_equal(.tr_series_estacoes(4L), c("T1", "T2", "T3", "T4"))
  expect_equal(.tr_series_estacoes(7L)[[7]], "7")
})

test_that("regressão soma de volta a série, e o sazonal tem média zero", {
  x <- serie_mensal()
  r <- tr_series_regression(x)
  expect_s3_class(r, "tr_series_reg")
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$resto), as.numeric(x), tolerance = 1e-8)
  expect_equal(mean(r$sazonal), 0, tolerance = 1e-8)
  expect_equal(stats::frequency(r$tendencia), 12)
  expect_equal(stats::start(r$resto), stats::start(x))
})

test_that("a decomposição não depende do contraste; a tabela de coeficientes sim", {
  x <- serie_mensal()
  a <- tr_series_regression(x, contraste = "soma_zero")
  b <- tr_series_regression(x, contraste = "categoria_base")
  expect_equal(as.numeric(a$sazonal), as.numeric(b$sazonal), tolerance = 1e-8)
  expect_equal(as.numeric(a$tendencia), as.numeric(b$tendencia), tolerance = 1e-8)
  expect_false(identical(names(stats::coef(a$ajuste)), names(stats::coef(b$ajuste))))
})

test_that("grau e sazonalidade ligam e desligam os blocos", {
  x <- serie_mensal()
  expect_equal(sum(tr_series_regression(x, sazonalidade = FALSE)$sazonal), 0)
  expect_length(stats::coef(tr_series_regression(x, grau = 3L)$ajuste), 1L + 3L + 11L)
  r0 <- tr_series_regression(x, grau = 0L)
  expect_equal(as.numeric(r0$tendencia), rep(mean(x), length(x)), tolerance = 1e-8)
})

test_that("o sazonal soma zero por ESTAÇÃO, mesmo com anos incompletos", {
  # Setembro a fevereiro: cada mês aparece um número diferente de vezes, e é aí
  # que centrar pela média da amostra deixa de centrar pela do ciclo.
  x <- stats::window(datasets::AirPassengers, start = c(1949, 9), end = c(1952, 2))
  r <- tr_series_regression(x)
  expect_equal(sum(tapply(r$sazonal, stats::cycle(r$sazonal), mean)), 0, tolerance = 1e-8)
  expect_equal(as.numeric(r$tendencia + r$sazonal + r$resto), as.numeric(x), tolerance = 1e-8)
})

test_that("regressão recusa série sem graus de liberdade para o modelo", {
  # Quatro observações, frequência 2, grau 2: 4 parâmetros para 4 pontos. O
  # ajuste roda, nenhum coeficiente falta, e sai R² ajustado NaN com uma
  # decomposição de aparência perfeita — é o resultado errado com cara de certo.
  expect_error(tr_series_regression(stats::ts(c(3, 7, 4, 9), frequency = 2, start = c(2000, 1)),
                                    grau = 2L),
               class = "tr_series_error_too_short")
  # Raspando por baixo: 3 parâmetros, 24 observações, e passa.
  expect_s3_class(tr_series_regression(stats::ts(1:24, frequency = 2), grau = 1L),
                  "tr_series_reg")
})

test_that("regressão recusa modelo vazio, série anual com sazonal, faltante e regressor", {
  x <- serie_mensal()
  expect_error(tr_series_regression(x, grau = 0L, sazonalidade = FALSE),
               class = "tr_series_error_empty_model")
  expect_error(tr_series_regression(serie_anual()), class = "tr_series_error_no_season")
  expect_error(tr_series_regression(datasets::presidents), class = "tr_series_error_missing_values")
  expect_error(tr_series_regression(x, regressor = x), class = "tr_series_error_xreg_unsupported")
  expect_error(tr_series_regression(x, grau = 4L), class = "tr_series_error_bad_option")
  expect_error(tr_series_regression(x, contraste = "meio"), class = "tr_series_error_bad_option")
})
