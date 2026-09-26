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

# ---- Resposta gradual: ω/(1 − δB) (Box & Tiao 1975) ---------------------------
# Oráculo: TSA::arimax(transfer = list(c(1, 0)), method = "ML") — o ajuste de
# função de transferência de Cryer & Chan (2008, cap. 11) — no airmiles do TSA
# (log), ARIMA(0,1,1)(0,1,1)12, intervenção na observação 69 (2001-09). Os
# números abaixo foram RODADOS no TSA 1.3.1 (não copiados do livro) e ficam
# fixos aqui: carregar o TSA sobrescreve `fitted.Arima` do forecast e
# contaminaria os testes seguintes, então só os dados vêm dele (sem
# carregar o namespace: `load()` do .rda). O TSA otimiza tudo junto por optim; aqui δ é perfilado
# — diferença medida < 3e-5 nos coeficientes e < 1e-5 nos erros-padrão.
# Tolerância declarada: 1e-3.
air <- function() {
  # Não `skip_if_not_installed`, que CARREGA o namespace.
  if (!nzchar(system.file(package = "TSA"))) skip("TSA não instalado (só os dados vêm dele)")
  e <- new.env()
  load(system.file("data", "airmiles.rda", package = "TSA"), envir = e)
  log(e$airmiles)
}
g <- function(t, termo, col) t[[col]][t$termo == termo]

test_that("degrau gradual no airmiles reproduz o TSA::arimax", {
  x <- air()
  t <- tr_series_intervencao(x, data = "2001, 9", tipo = "degrau", p = 0L, d = 1L, q = 1L,
                             P = 0L, D = 1L, Q = 1L, resposta = "gradual")
  # TSA 1.3.1: coef(ma1, sma1, S-AR1, S-MA0) e sqrt(diag(var.coef)).
  ref <- c(ma1 = -0.44139953, sma1 = -0.7382725, delta = -0.29605395, intervencao = -0.35892233)
  ref_se <- c(ma1 = 0.090245144, sma1 = 0.13876333, delta = 0.070814469, intervencao = 0.03311956)
  for (k in names(ref)) {
    expect_equal(g(t, k, "estimativa"), ref[[k]], tolerance = 1e-3, info = k)
    expect_equal(g(t, k, "erro_padrao"), ref_se[[k]], tolerance = 1e-3, info = k)
  }
  expect_equal(g(t, "efeito_longo_prazo", "estimativa"),
               g(t, "intervencao", "estimativa") / (1 - g(t, "delta", "estimativa")), tolerance = 1e-12)
  expect_equal(t$termo[1:3], c("intervencao", "delta", "efeito_longo_prazo"))
  expect_false("TSA" %in% loadedNamespaces())
})

test_that("pulso gradual no airmiles reproduz o TSA::arimax", {
  x <- air()
  t <- tr_series_intervencao(x, data = "2001, 9", tipo = "pulso", p = 0L, d = 1L, q = 1L,
                             P = 0L, D = 1L, Q = 1L, resposta = "gradual")
  ref <- c(ma1 = -0.5045245028, sma1 = -0.7434674780, delta = 0.6946808556, intervencao = -0.3459058970)
  ref_se <- c(ma1 = 0.08121181792, sma1 = 0.15185241395, delta = 0.06839809383, intervencao = 0.02851777767)
  for (k in names(ref)) {
    expect_equal(g(t, k, "estimativa"), ref[[k]], tolerance = 1e-3, info = k)
    expect_equal(g(t, k, "erro_padrao"), ref_se[[k]], tolerance = 1e-3, info = k)
  }
  expect_false("efeito_longo_prazo" %in% t$termo)
})

test_that("resposta gradual: rampa recusada; imediata sem mudança", {
  x <- sb()
  expect_error(tr_series_intervencao(x, data = "1983, 2", tipo = "rampa", resposta = "gradual"),
               class = "tr_series_error_bad_option")
  expect_error(tr_series_intervencao(x, data = "1983, 2", resposta = "lenta"),
               class = "tr_series_error_bad_option")
  a <- tr_series_intervencao(x, data = "1983, 2", p = 1L, d = 0L, q = 0L, P = 1L, D = 1L, Q = 1L)
  b <- tr_series_intervencao(x, data = "1983, 2", p = 1L, d = 0L, q = 0L, P = 1L, D = 1L, Q = 1L,
                             resposta = "imediata")
  expect_identical(a, b)
})
