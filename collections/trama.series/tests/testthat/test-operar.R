test_that("diferença simples e sazonal, com a sazonal recusando série anual", {
  x <- serie_mensal()
  expect_equal(as.numeric(tr_series_diff(x)), as.numeric(diff(x)))
  s <- tr_series_diff(x, tipo = "sazonal")
  expect_equal(length(s), length(x) - 12L)
  expect_equal(as.numeric(s)[[1]], x[[13]] - x[[1]])
  expect_equal(length(tr_series_diff(x, ordem = 2L)), length(x) - 2L)
  expect_error(tr_series_diff(serie_anual(), tipo = "sazonal"), class = "tr_series_error_no_season")
  expect_error(tr_series_diff(x, ordem = 0L), class = "tr_series_error_bad_option")
  expect_error(tr_series_diff(x, tipo = "anual"), class = "tr_series_error_bad_option")
  expect_error(tr_series_diff(stats::ts(1:2)), class = "tr_series_error_too_short")
})

test_that("lag desloca o tempo, não os valores", {
  x <- serie_mensal()
  l <- tr_series_lag(x, 3L)
  expect_equal(as.numeric(l), as.numeric(x))
  expect_equal(stats::tsp(l)[[1]], stats::tsp(x)[[1]] + 3 / 12)
  expect_equal(stats::tsp(tr_series_lag(x, -1L))[[1]], stats::tsp(x)[[1]] - 1 / 12)
})

test_that("transformações: log, raiz, Box-Cox automático e recusa de não positivo", {
  x <- serie_mensal()
  expect_equal(tr_series_transform(x, "log"), log(x))
  expect_equal(tr_series_transform(x, "raiz"), sqrt(x))
  bc <- tr_series_transform(x, "boxcox")
  expect_true(is.numeric(attr(bc, "lambda")))
  expect_equal(as.numeric(tr_series_transform(x, "boxcox", lambda = "0")), as.numeric(log(x)))
  expect_equal(attr(tr_series_transform(x, "boxcox", lambda = "0,5"), "lambda"), 0.5)
  expect_error(tr_series_transform(x, "boxcox", lambda = "meio"), class = "tr_series_error_bad_option")
  z <- stats::ts(c(0, 1, 2))
  err <- tryCatch(tr_series_transform(z, "log"), condition = identity)
  expect_s3_class(err, "tr_series_error_nonpositive")
  expect_match(conditionMessage(err), "1 abaixo")
  expect_equal(as.numeric(tr_series_transform(z, "raiz")), sqrt(c(0, 1, 2)))
})

test_that("janela: fim só com o ano vai até dezembro, e fora da série é erro", {
  x <- serie_mensal()
  w <- tr_series_window(x, "1955, 1", "1956")
  expect_equal(length(w), 24L)
  expect_equal(stats::start(w), c(1955, 1))
  expect_equal(stats::end(w), c(1956, 12))
  expect_identical(tr_series_window(x), x)
  expect_equal(stats::start(tr_series_window(x, inicio = "1958")), c(1958, 1))
  expect_error(tr_series_window(x, "1940"), class = "tr_series_error_bad_period")
  expect_error(tr_series_window(x, fim = "1995"), class = "tr_series_error_bad_period")
  expect_error(tr_series_window(x, "1957", "1956"), class = "tr_series_error_bad_period")
  expect_error(tr_series_window(x, "abc"), class = "tr_series_error_bad_period")
})

test_that("média móvel centrada de ordem 12 perde meia janela em cada ponta", {
  m <- tr_series_moving_average(serie_mensal(), 12L)
  expect_equal(length(m), length(serie_mensal()))
  expect_equal(sum(is.na(m)), 12L)
  expect_error(tr_series_moving_average(serie_mensal(), 1L), class = "tr_series_error_bad_option")
})

test_that("agregar respeita o calendário e descarta blocos incompletos", {
  a <- tr_series_aggregate(serie_mensal(), 1L, "soma")
  expect_equal(stats::start(a), c(1949, 1))
  expect_equal(a[[1]], sum(serie_mensal()[1:12]))
  expect_equal(length(a), 12L)
  # Começa em março: o primeiro "ano" seria de mar a fev com stats::aggregate.
  x <- stats::ts(1:30, start = c(2000, 3), frequency = 12)
  b <- tr_series_aggregate(x, 1L, "soma")
  expect_equal(stats::start(b), c(2001, 1))
  expect_equal(as.numeric(b), sum(11:22))
  tri <- tr_series_aggregate(x, 4L, "media")
  expect_equal(stats::start(tri), c(2000, 2))
  expect_equal(tri[[1]], mean(2:4))
  expect_error(tr_series_aggregate(serie_mensal(), 5L), class = "tr_series_error_bad_frequency")
  expect_error(tr_series_aggregate(serie_mensal(), 12L), class = "tr_series_error_bad_frequency")
})

test_that("interpolação preenche faltante e deixa série completa intacta", {
  p <- datasets::presidents
  expect_true(anyNA(p))
  expect_false(anyNA(tr_series_interpolate(p)))
  expect_identical(tr_series_interpolate(serie_mensal()), serie_mensal())
  expect_error(tr_series_interpolate(stats::ts(c(NA, 1, NA))), class = "tr_series_error_too_short")
})

test_that("detrend linear: reta pura + ruído sai com média e inclinação zero", {
  set.seed(42)
  x <- stats::ts(10 + 0.5 * (1:120) + stats::rnorm(120), start = c(2000, 1), frequency = 12)
  s <- tr_series_detrend(x, "linear")
  expect_equal(stats::tsp(s), stats::tsp(x))
  expect_equal(mean(s), 0, tolerance = 1e-8)
  expect_equal(unname(stats::coef(stats::lm(as.numeric(s) ~ seq_along(s)))[[2]]), 0,
               tolerance = 1e-8)
  # A tendência guardada reconstrói a série, e a inclinação dela é a da reta.
  tend <- attr(s, "tendencia")
  expect_equal(as.numeric(s + tend), as.numeric(x))
  expect_equal(unname(diff(as.numeric(tend))[[1]]), 0.5, tolerance = .02)
})

test_that("detrend polinomial e loess tiram a curva; faltante fica faltante", {
  tt <- 1:96
  x <- stats::ts(0.02 * tt^2 + sin(2 * pi * tt / 12), frequency = 12)
  p <- tr_series_detrend(x, "polinomial", grau = 2L)
  expect_equal(unname(stats::coef(stats::lm(as.numeric(p) ~ poly(tt, 2)))[2:3]), c(0, 0),
               tolerance = 1e-8)
  # A sazonalidade fica: o que sobra é o seno.
  expect_gt(stats::cor(as.numeric(p), sin(2 * pi * tt / 12)), .99)
  l <- tr_series_detrend(x, "loess", suavidade = .5)
  expect_lt(abs(mean(l)), .1)
  xn <- x; xn[10] <- NA
  ln <- tr_series_detrend(xn, "linear")
  expect_true(is.na(ln[10])); expect_equal(sum(is.na(ln)), 1L)
  expect_false(anyNA(attr(ln, "tendencia")))
  expect_error(tr_series_detrend(x, "polinomial", grau = 6L), class = "tr_series_error_bad_option")
  expect_error(tr_series_detrend(x, "loess", suavidade = 0), class = "tr_series_error_bad_option")
  expect_error(tr_series_detrend(x, "cubica"), class = "tr_series_error_bad_option")
  expect_error(tr_series_detrend(stats::ts(1:3), "polinomial", grau = 2L),
               class = "tr_series_error_too_short")
})

test_that("detrend por diferença perde a 1ª observação e mantém o calendário", {
  x <- serie_mensal()
  d <- tr_series_detrend(x, "diferenca")
  expect_equal(length(d), length(x) - 1L)
  expect_equal(stats::start(d), c(1949, 2))
  expect_equal(stats::end(d), stats::end(x))
  expect_equal(as.numeric(d), as.numeric(diff(x)))
  expect_equal(as.numeric(d + attr(d, "tendencia")), as.numeric(x)[-1])
})
