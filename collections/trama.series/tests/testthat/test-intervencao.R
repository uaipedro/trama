# Intervenções: declaradas por `series/intervencao`, estimadas no `series/arima`.
#
# Oráculos: imediatas contra `forecast::Arima(xreg = )` com o mesmo regressor
# (Box & Tiao 1975, forma de ordem zero), a 1e-8; gradual contra o
# `TSA::arimax(transfer = list(c(1, 0)))` (Cryer & Chan 2008, cap. 11), a
# 1e-3; inovacional contra o regressor do `tsoutliers::outliers.effects()`
# (Chen & Liu 1993), a 1e-8; detecção contra o `tsoutliers::tso()` e o degrau
# do Nilo em 1899 (Cobb 1978).
sb <- function() log(datasets::Seatbelts[, "drivers"])
coefs <- function(m) trama.models::tr_models_coefs(tr_series_as_fit(m))$tabela
g <- function(t, termo, col) t[[col]][t$termo == termo]

test_that("declarar não ajusta: devolve a mesma série, com as intervenções em ordem", {
  x <- sb()
  s <- tr_series_intervencao(x, data = "1983, 2", tipo = "degrau")
  s <- tr_series_intervencao(s, data = "1975, 1", tipo = "pulso")
  expect_identical(as.numeric(s), as.numeric(x))
  expect_identical(stats::tsp(s), stats::tsp(x))
  l <- .tr_series_intervencoes(s, "teste")
  expect_equal(vapply(l, `[[`, "", "termo"), c("degrau_1983_fev", "pulso_1975_jan"))
  expect_equal(vapply(l, `[[`, 0, "indice"), c(170, 73))
  # O card do ajuste de cima não passa para a intervenção.
  d <- tr_series_deseasonalize(x)$out
  expect_false(is.null(.tr_series_card_ajuste(d)))
  expect_null(.tr_series_card_ajuste(tr_series_intervencao(d, "1983, 2")))
})

test_that("degrau em 1983-02 no Seatbelts: o regressor é a coluna law, e o ajuste é o do forecast::Arima", {
  x <- sb()
  m <- tr_series_arima(tr_series_intervencao(x, data = "1983, 2", tipo = "degrau"), automatico = FALSE,
                       p = 1L, d = 0L, q = 0L, P = 1L, D = 1L, Q = 1L, constante = FALSE)
  law <- as.numeric(datasets::Seatbelts[, "law"])
  expect_identical(unname(m$xreg[, 1]), law)
  ref <- forecast::Arima(x, order = c(1, 0, 0), seasonal = c(1, 1, 1), xreg = cbind(law = law),
                         include.constant = FALSE)
  expect_equal(unname(stats::coef(m)), unname(stats::coef(ref)), tolerance = 1e-8)
  t <- coefs(m)
  expect_equal(g(t, "degrau_1983_fev", "estimativa"), unname(stats::coef(ref)[["law"]]), tolerance = 1e-8)
  expect_equal(g(t, "degrau_1983_fev", "erro_padrao"), unname(sqrt(diag(ref$var.coef))[["law"]]),
               tolerance = 1e-8)
})

test_that("pulso e rampa montam o regressor certo, e várias intervenções entram juntas", {
  x <- stats::ts(as.numeric(datasets::Nile))
  s <- tr_series_intervencao(tr_series_intervencao(x, "30", "rampa"), "43", "pulso")
  m <- tr_series_arima(s, automatico = FALSE, p = 1L, d = 0L, q = 1L, constante = TRUE)
  X <- cbind(rampa = pmax(0, seq_along(x) - 29), pulso = as.numeric(seq_along(x) == 43))
  ref <- forecast::Arima(x, order = c(1, 0, 1), xreg = X, include.constant = TRUE)
  expect_equal(unname(stats::coef(m)), unname(stats::coef(ref)), tolerance = 1e-8)
  expect_equal(unname(sqrt(diag(m$var.coef))), unname(sqrt(diag(ref$var.coef))), tolerance = 1e-8)
})

test_that("a previsão estende cada efeito: pulso a zero, degrau em 1, rampa subindo", {
  x <- stats::ts(as.numeric(datasets::Nile), start = 1871)
  s <- tr_series_intervencao(tr_series_intervencao(x, "1899", "degrau"), "1913", "pulso")
  s <- tr_series_intervencao(s, "1950", "rampa")
  m <- tr_series_arima(s, automatico = FALSE, p = 1L, d = 0L, q = 0L, constante = TRUE)
  fx <- .tr_series_interv_futuro(m, 3L)
  expect_equal(unname(fx), cbind(c(1, 1, 1), c(0, 0, 0), c(22, 23, 24)))
  ref <- forecast::forecast(m, h = 3, xreg = fx, level = c(80, 95))
  expect_equal(tr_series_forecast(m, horizonte = 3L)$mean, ref$mean, tolerance = 1e-12)
})

# ---- Gradual: ω/(1 − δB) (Box & Tiao 1975) -----------------------------------
# Oráculo: TSA::arimax(transfer = list(c(1, 0)), method = "ML") no airmiles
# (log), ARIMA(0,1,1)(0,1,1)12, intervenção em 2001-09. Números RODADOS no TSA
# 1.3.1 e fixos aqui: carregar o TSA sobrescreve `fitted.Arima` do forecast,
# então só os dados vêm dele (`load()` do .rda, sem carregar o namespace). O
# TSA otimiza tudo junto; aqui δ é perfilado (diferença < 3e-5). Tolerância 1e-3.
air <- function() {
  if (!nzchar(system.file(package = "TSA"))) skip("TSA não instalado (só os dados vêm dele)")
  e <- new.env()
  load(system.file("data", "airmiles.rda", package = "TSA"), envir = e)
  log(e$airmiles)
}
air_gradual <- function(tipo) {
  tr_series_arima(tr_series_intervencao(air(), "2001, 9", tipo, dinamica = "gradual"), automatico = FALSE,
                  p = 0L, d = 1L, q = 1L, P = 0L, D = 1L, Q = 1L, constante = FALSE)
}

test_that("degrau gradual no airmiles reproduz o TSA::arimax", {
  m <- air_gradual("degrau")
  t <- coefs(m)
  w <- "degrau_gradual_2001_set"
  ref <- stats::setNames(c(-0.44139953, -0.7382725, -0.29605395, -0.35892233), c("ma1", "sma1", "delta", w))
  ref_se <- stats::setNames(c(0.090245144, 0.13876333, 0.070814469, 0.03311956), names(ref))
  for (k in names(ref)) {
    expect_equal(g(t, k, "estimativa"), ref[[k]], tolerance = 1e-3, info = k)
    expect_equal(g(t, k, "erro_padrao"), ref_se[[k]], tolerance = 1e-3, info = k)
  }
  expect_equal(g(t, "efeito_longo_prazo", "estimativa"),
               g(t, w, "estimativa") / (1 - g(t, "delta", "estimativa")), tolerance = 1e-12)
  # δ conta como parâmetro: AIC = −2 ll + 2 (coefs + δ + σ²).
  expect_equal(m$aic, -2 * m$loglik + 2 * (length(stats::coef(m)) + 2), tolerance = 1e-12)
  expect_equal(trama.models::tr_models_loglik(tr_series_as_fit(m))$k, length(stats::coef(m)) + 2)
  expect_equal(.tr_series_modelo_resumo(m)$aic, round(m$aic, 2))
  expect_false("TSA" %in% loadedNamespaces())
})

test_that("pulso gradual no airmiles reproduz o TSA::arimax, e a previsão decai à razão δ", {
  m <- air_gradual("pulso")
  t <- coefs(m)
  w <- "pulso_gradual_2001_set"
  ref <- stats::setNames(c(-0.5045245028, -0.7434674780, 0.6946808556, -0.3459058970), c("ma1", "sma1", "delta", w))
  ref_se <- stats::setNames(c(0.08121181792, 0.15185241395, 0.06839809383, 0.02851777767), names(ref))
  for (k in names(ref)) {
    expect_equal(g(t, k, "estimativa"), ref[[k]], tolerance = 1e-3, info = k)
    expect_equal(g(t, k, "erro_padrao"), ref_se[[k]], tolerance = 1e-3, info = k)
  }
  expect_false("efeito_longo_prazo" %in% t$termo)
  fx <- .tr_series_interv_futuro(m, 2L)[, 1]
  n <- length(m$x); i0 <- 69
  dl <- m$tr_intervencoes$gradual$delta
  expect_equal(fx, dl^(n + 1:2 - i0), tolerance = 1e-12)
})

# ---- Inovacional (Fox 1972; Chen & Liu 1993) ---------------------------------

test_that("inovacional: o regressor é o do tsoutliers com os parâmetros finais, e o ajuste é o ponto fixo", {
  skip_if_not_installed("tsoutliers")
  y <- stats::ts(as.numeric(datasets::Nile), start = 1871)
  for (o in list(c(1, 1, 1), c(2, 0, 0))) {
    m <- tr_series_arima(tr_series_intervencao(y, "1913", "inovacional"), automatico = FALSE,
                         p = o[[1]], d = o[[2]], q = o[[3]], constante = o[[2]] == 0)
    mo <- data.frame(type = factor("IO"), ind = 43, coefhat = 1)
    oe <- tsoutliers::outliers.effects(mo, length(y), pars = tsoutliers::coefs2poly(m))
    expect_equal(unname(m$xreg[, "inovacional_1913"]), unname(oe[, 1]), tolerance = 1e-8)
    ref <- forecast::Arima(y, order = o, xreg = oe, include.constant = o[[2]] == 0)
    expect_equal(unname(stats::coef(m)), unname(stats::coef(ref)), tolerance = 1e-6)
  }
  # Sazonal com diferenças: o ψ inclui (1 − B)(1 − B^12).
  x <- sb()
  m <- tr_series_arima(tr_series_intervencao(x, "1983, 2", "inovacional"), automatico = FALSE,
                       p = 1L, d = 1L, q = 0L, P = 0L, D = 1L, Q = 1L, constante = FALSE)
  mo <- data.frame(type = factor("IO"), ind = 170, coefhat = 1)
  oe <- tsoutliers::outliers.effects(mo, length(x), pars = tsoutliers::coefs2poly(m))
  expect_equal(unname(m$xreg[, 1]), unname(oe[, 1]), tolerance = 1e-8)
})

test_that("inovacional num SARIMA completo para no ruído do otimizador, não em erro", {
  x <- log(datasets::AirPassengers)
  m <- tr_series_arima(tr_series_intervencao(x, "1955, 3", "inovacional"), automatico = FALSE,
                       p = 1L, d = 1L, q = 1L, P = 1L, D = 1L, Q = 1L, constante = FALSE)
  expect_true("inovacional_1955_mar" %in% names(stats::coef(m)))
  re <- forecast::Arima(x, order = c(1, 1, 1), seasonal = c(1, 1, 1), xreg = m$xreg, include.constant = FALSE)
  expect_equal(unname(stats::coef(m)), unname(stats::coef(re)), tolerance = 1e-3)
})

test_that("bootstrap com intervenção e modelos que não as estimam são recusados com aviso", {
  s <- tr_series_intervencao(sb(), "1983, 2")
  m <- tr_series_arima(s, automatico = FALSE, p = 1L, d = 0L, q = 0L, P = 1L, D = 1L, Q = 1L, constante = FALSE)
  expect_error(tr_series_forecast(m, intervalo = "bootstrap"), class = "tr_series_error_bad_option")
  expect_error(tr_series_ets(s), "não estima intervenções", class = "tr_series_error_bad_option")
  expect_error(tr_series_holt_winters(s), class = "tr_series_error_bad_option")
  expect_error(tr_series_regression(s), class = "tr_series_error_bad_option")
  expect_match(utils::capture.output(print(tr_series_arima(s)))[[1]], "Series: serie")
})

test_that("automático com intervenções: escolhe a ordem e estima os efeitos", {
  s <- tr_series_intervencao(sb(), "1983, 2", "degrau")
  m <- tr_series_arima(s)
  expect_true("degrau_1983_fev" %in% names(stats::coef(m)))
  expect_false(is.null(m$tr_intervencoes))
  mi <- tr_series_arima(tr_series_intervencao(s, "1975, 1", "inovacional"))
  expect_true("inovacional_1975_jan" %in% names(stats::coef(mi)))
  expect_match(tr_series_as_fit(mi)$rotulo, "2 intervenções")
})

test_that("bordas: data fora, duplicada, gradual onde não cabe, carimbo vencido", {
  x <- sb()
  expect_error(tr_series_intervencao(x, data = "1990, 1"), class = "tr_series_error_bad_period")
  expect_error(tr_series_intervencao(x, data = "1969, 1"), class = "tr_series_error_bad_period")
  expect_error(tr_series_intervencao(x, data = "1983, 2", tipo = "salto"), class = "tr_series_error_bad_option")
  expect_error(tr_series_intervencao(x, "1983, 2", "rampa", dinamica = "gradual"),
               class = "tr_series_error_bad_option")
  expect_error(tr_series_intervencao(datasets::presidents, data = "1960, 1"),
               class = "tr_series_error_missing_values")
  s <- tr_series_intervencao(x, "1983, 2")
  expect_error(tr_series_intervencao(s, "1983, 2"), class = "tr_series_error_bad_option")
  expect_error(tr_series_intervencao(tr_series_intervencao(x, "1983, 2", dinamica = "gradual"),
                                     "1975, 1", "pulso", dinamica = "gradual"),
               class = "tr_series_error_bad_option")
  expect_error(tr_series_arima(exp(s), automatico = FALSE, p = 1L, d = 0L, q = 0L),
               "transformação", class = "tr_series_error_bad_option")
})

# Várias no mesmo bloco: o oráculo é o encadeamento, cujo ajuste já confere
# com o `forecast::Arima(xreg = )` acima. Mesmo carimbo, mesmo ajuste.
test_that("várias datas por ';' e tabela de datas valem o mesmo que encadear blocos", {
  x <- sb()
  enc <- tr_series_intervencao(tr_series_intervencao(x, "1975, 1", "degrau"), "1983, 2", "degrau")
  um <- tr_series_intervencao(x, "1975, 1; 1983, 2", "degrau")
  expect_identical(attr(um, "intervencoes"), attr(enc, "intervencoes"))
  tab <- tibble::tibble(quando = c("1975 jan", NA, "1983, 2"))
  expect_identical(attr(tr_series_intervencao(x, datas = tab, tempo = "quando"), "intervencoes"),
                   attr(enc, "intervencoes"))
  datas <- tibble::tibble(data = as.Date(c("1975-01-01", "1983-02-15")))
  expect_identical(attr(tr_series_intervencao(x, datas = datas), "intervencoes"), attr(enc, "intervencoes"))
  # Param e tabela se somam, na ordem: as do param primeiro. Tabela sem
  # coluna `tipo` usa o param, então tudo sai pulso.
  s <- tr_series_intervencao(x, "1970, 6", "pulso", datas = datas)
  expect_equal(vapply(.tr_series_intervencoes(s, "t"), `[[`, "", "termo"),
               c("pulso_1970_jun", "pulso_1975_jan", "pulso_1983_fev"))
  ord <- list(automatico = FALSE, p = 1L, d = 0L, q = 0L, P = 1L, D = 1L, Q = 1L, constante = FALSE)
  expect_equal(stats::coef(do.call(tr_series_arima, c(list(um), ord))),
               stats::coef(do.call(tr_series_arima, c(list(enc), ord))), tolerance = 1e-10)
})

test_that("tabela da detecção liga direto: data, tipo e temporária como pulso gradual", {
  skip_if_not_installed("tsoutliers")
  y <- datasets::Nile
  det <- tr_series_detect_interventions(y)
  s <- tr_series_intervencao(y, datas = det)
  l <- .tr_series_intervencoes(s, "t")
  esperado <- ifelse(det$tipo == "temporaria", "pulso", det$tipo)
  expect_equal(vapply(l, `[[`, "", "tipo"), esperado)
  expect_equal(vapply(l, `[[`, 0, "indice"), det$indice)
  expect_equal(vapply(l, `[[`, "", "dinamica"), ifelse(det$tipo == "temporaria", "gradual", "imediata"))
  # Trimestral: o rótulo "1960 T3" que o pacote escreve volta a ser lido.
  q <- datasets::UKgas
  i <- tr_series_intervencao(q, datas = tibble::tibble(data = "1975 T3", tipo = "pulso"))
  expect_equal(.tr_series_intervencoes(i, "t")[[1]]$termo, "pulso_1975_T3")
})

test_that("várias no bloco: bordas", {
  x <- sb()
  expect_error(tr_series_intervencao(x), class = "tr_series_error_blank_param")
  expect_error(tr_series_intervencao(x, "1975, 1; 1975, 1"), class = "tr_series_error_bad_option")
  expect_error(tr_series_intervencao(x, datas = tibble::tibble(dia = "1975, 1")),
               class = "tr_series_error_unknown_column")
  expect_error(tr_series_intervencao(x, datas = tibble::tibble(data = "1975, 1", tipo = "salto")),
               class = "tr_series_error_bad_option")
  expect_error(tr_series_intervencao(x, datas = tibble::tibble(data = c("1975, 1", "1980, 1"),
                                                             tipo = "temporaria")),
               "temporária", class = "tr_series_error_bad_option")
  expect_error(tr_series_intervencao(x, "1990, 1; 1975, 1"), class = "tr_series_error_bad_period")
})

test_that("migração da v2 para a v3 não mexe nos params", {
  no <- Filter(function(n) n$id == "series/intervencao", trama_collection()$nodes)[[1]]
  v2 <- list(data = "1983, 2", tipo = "pulso", dinamica = "gradual")
  expect_identical(no$migracoes[["3"]](v2), v2)
})

test_that("migração da v1: ordens saem, resposta vira dinamica", {
  no <- Filter(function(n) n$id == "series/intervencao", trama_collection()$nodes)[[1]]
  v1 <- list(data = "1983, 2", tipo = "pulso", p = 1L, d = 0L, q = 0L, P = 1L, D = 1L, Q = 1L,
             constante = FALSE, resposta = "gradual")
  expect_equal(no$migracoes[["2"]](v1), list(data = "1983, 2", tipo = "pulso", dinamica = "gradual"))
})

# ---- Detecção (Chen & Liu 1993, via tsoutliers::tso) -------------------------

test_that("detecção no Nilo: a mesma resposta do tso, e o degrau de 1899", {
  skip_if_not_installed("tsoutliers")
  y <- datasets::Nile
  t <- tr_series_detect_interventions(y)
  ref <- tsoutliers::tso(y)$outliers
  expect_equal(t$indice, ref$ind)
  expect_equal(t$sigla, as.character(ref$type))
  expect_equal(t$efeito, ref$coefhat, tolerance = 1e-12)
  expect_equal(t$t, ref$tstat, tolerance = 1e-12)
  ls <- t[t$tipo == "degrau", ]
  expect_equal(ls$data, "1899")
  expect_equal(ls$efeito, -242.2289, tolerance = 1e-4)
})

test_that("detecção com modelo ligado, tipos, valor crítico e série sem candidatos", {
  skip_if_not_installed("tsoutliers")
  y <- datasets::Nile
  mod <- forecast::Arima(y, order = c(0, 1, 1))
  t <- tr_series_detect_interventions(y, modelo = mod, inovacional = TRUE, valor_critico = 3.5)
  ref <- tsoutliers::tso(y, types = c("AO", "LS", "TC", "IO"), cval = 3.5, tsmethod = "arima",
                         args.tsmethod = list(order = c(0, 1, 1), seasonal = list(order = c(0, 0, 0), period = 1),
                                              include.mean = FALSE))$outliers
  expect_equal(t$indice, ref$ind)
  expect_equal(t$efeito, ref$coefhat, tolerance = 1e-12)
  expect_error(tr_series_detect_interventions(y, pulso = FALSE, degrau = FALSE, temporaria = FALSE),
               class = "tr_series_error_bad_option")
  set.seed(1)
  z <- stats::ts(stats::rnorm(60))
  vazio <- tr_series_detect_interventions(z, valor_critico = 6)
  expect_equal(nrow(vazio), 0L)
  expect_named(vazio, c("data", "tipo", "sigla", "indice", "efeito", "t"))
})
