# Diagnóstico dos resíduos de VAR e VECM. Oráculo: o próprio `vars` (a estatística
# e o p-valor saem iguais, a 1e-10), e para o Ljung-Box multivariado uma conta
# feita à mão pela fórmula de Hosking (1980), a 1e-8.

data(Canada, package = "vars")
var2 <- .tr_series_var(vars::VAR(Canada, p = 2, type = "const"), Canada, "VAR")

# Hosking (1980), eq. do Ljung-Box ajustado, com loops explícitos:
#   C_j = (1/T) sum_{t=j+1}^{T} u_t u_{t-j}'
#   Q_h = T^2 sum_{j=1}^{h} tr(C_j' C_0^{-1} C_j C_0^{-1}) / (T - j)
# (Box-Pierce: Q_h = T sum_j tr(...), sem o fator T/(T - j)).
hosking_mao <- function(u, h, ajustado = TRUE) {
  T <- nrow(u); K <- ncol(u)
  C0 <- matrix(0, K, K)
  for (t in 1:T) C0 <- C0 + tcrossprod(u[t, ])
  C0 <- C0 / T
  C0inv <- solve(C0)
  q <- 0
  for (j in 1:h) {
    Cj <- matrix(0, K, K)
    for (t in (j + 1):T) Cj <- Cj + tcrossprod(u[t, ], u[t - j, ])
    Cj <- Cj / T
    traco <- sum(diag(t(Cj) %*% C0inv %*% Cj %*% C0inv))
    q <- q + if (ajustado) T * T * traco / (T - j) else T * traco
  }
  q
}

test_that("Ljung-Box multivariado = vars PT.adjusted no Canada VAR(2), a 1e-10", {
  # Tolerância 1e-10 (relativa): mesmo cálculo, só o caminho de chamada muda.
  r <- tr_series_portmanteau_mv(var2, metodo = "ljung_box", defasagens = 16L)
  ref <- vars::serial.test(var2$ajuste, lags.pt = 16, type = "PT.adjusted")$serial
  expect_equal(r$estatistica, unname(ref$statistic), tolerance = 1e-10)
  expect_equal(r$p_valor, unname(ref$p.value), tolerance = 1e-10)
  expect_equal(r$extra$graus_liberdade, unname(ref$parameter))
  # K² (h − p) = 16 × (16 − 2) = 224.
  expect_equal(r$extra$graus_liberdade, 224)
})

test_that("Ljung-Box multivariado = conta à mão de Hosking (1980), a 1e-8", {
  u <- stats::residuals(var2$ajuste)
  q_mao <- hosking_mao(u, h = 12, ajustado = TRUE)
  r <- tr_series_portmanteau_mv(var2, metodo = "ljung_box", defasagens = 12L)
  expect_equal(r$estatistica, q_mao, tolerance = 1e-8)
  # E o p-valor, pela qui-quadrado com K² (h − p) = 16 × 10 = 160 graus de liberdade.
  expect_equal(r$p_valor, pchisq(q_mao, df = 160, lower.tail = FALSE), tolerance = 1e-8)
})

test_that("Box-Pierce = vars PT.asymptotic, e a conta à mão sem o fator T/(T - j)", {
  r <- tr_series_portmanteau_mv(var2, metodo = "box_pierce", defasagens = 16L)
  ref <- vars::serial.test(var2$ajuste, lags.pt = 16, type = "PT.asymptotic")$serial
  expect_equal(r$estatistica, unname(ref$statistic), tolerance = 1e-10)
  expect_equal(r$p_valor, unname(ref$p.value), tolerance = 1e-10)
  u <- stats::residuals(var2$ajuste)
  expect_equal(r$estatistica, hosking_mao(u, h = 16, ajustado = FALSE), tolerance = 1e-8)
})

test_that("defasagens = 0 usa h = min(16, T/5), e a regra recusa h sem graus de liberdade", {
  # Canada: 82 resíduos, T/5 = 16, e o teto de 16 é o padrão do vars.
  expect_equal(.tr_series_portmanteau_h_auto(82), 16L)
  expect_equal(.tr_series_portmanteau_h_auto(40), 8L)
  expect_equal(.tr_series_portmanteau_h_auto(400), 16L)
  auto <- tr_series_portmanteau_mv(var2)
  expect_equal(auto$extra$defasagens, 16L)
  ref <- vars::serial.test(var2$ajuste, lags.pt = 16, type = "PT.adjusted")$serial
  expect_equal(auto$estatistica, unname(ref$statistic), tolerance = 1e-10)
  expect_error(tr_series_portmanteau_mv(var2, defasagens = 2L),
               class = "tr_series_error_bad_option")
})

test_that("Jarque-Bera multivariado = vars, com assimetria e curtose separadas", {
  r <- tr_series_normality_mv(var2)
  jb <- vars::normality.test(var2$ajuste, multivariate.only = TRUE)$jb.mul
  expect_equal(r$estatistica, as.numeric(jb$JB$statistic), tolerance = 1e-10)
  expect_equal(r$p_valor, as.numeric(jb$JB$p.value), tolerance = 1e-10)
  expect_equal(r$extra$assimetria$estatistica, as.numeric(jb$Skewness$statistic), tolerance = 1e-10)
  expect_equal(r$extra$assimetria$p_valor, as.numeric(jb$Skewness$p.value), tolerance = 1e-10)
  expect_equal(r$extra$curtose$estatistica, as.numeric(jb$Kurtosis$statistic), tolerance = 1e-10)
  expect_equal(r$extra$curtose$p_valor, as.numeric(jb$Kurtosis$p.value), tolerance = 1e-10)
  # A soma das partes é a estatística conjunta (2K graus de liberdade).
  expect_equal(r$estatistica, r$extra$assimetria$estatistica + r$extra$curtose$estatistica,
               tolerance = 1e-10)
})

test_that("ARCH multivariado = vars arch.test, com a defasagem pedida", {
  for (k in c(5L, 3L)) {
    r <- tr_series_arch_mv(var2, defasagens = k)
    ref <- vars::arch.test(var2$ajuste, lags.multi = k, multivariate.only = TRUE)$arch.mul
    expect_equal(r$estatistica, unname(ref$statistic), tolerance = 1e-10)
    expect_equal(r$p_valor, unname(ref$p.value), tolerance = 1e-10)
    expect_equal(r$extra$graus_liberdade, unname(ref$parameter))
  }
})

test_that("os três testes valem também para o VECM (vec2var), a 1e-10", {
  data(denmark, package = "urca")
  den <- stats::ts(as.matrix(denmark[, c("LRM", "LRY", "IBO", "IDE")]), start = c(1974, 1), frequency = 4)
  jo <- urca::ca.jo(den, type = "trace", ecdet = "const", K = 2, spec = "longrun")
  vec <- vars::vec2var(jo, r = 1)
  v <- .tr_series_var(vec, den, "VECM", vecm = list(posto = 1L))
  pt <- tr_series_portmanteau_mv(v, defasagens = 12L)
  ref <- vars::serial.test(vec, lags.pt = 12, type = "PT.adjusted")$serial
  expect_equal(pt$estatistica, unname(ref$statistic), tolerance = 1e-10)
  expect_equal(pt$p_valor, unname(ref$p.value), tolerance = 1e-10)
  jb <- tr_series_normality_mv(v)
  refjb <- vars::normality.test(vec, multivariate.only = TRUE)$jb.mul
  expect_equal(jb$estatistica, as.numeric(refjb$JB$statistic), tolerance = 1e-10)
  expect_equal(jb$p_valor, as.numeric(refjb$JB$p.value), tolerance = 1e-10)
  arch <- tr_series_arch_mv(v, defasagens = 5L)
  refa <- vars::arch.test(vec, lags.multi = 5, multivariate.only = TRUE)$arch.mul
  expect_equal(arch$estatistica, unname(refa$statistic), tolerance = 1e-10)
  expect_equal(arch$p_valor, unname(refa$p.value), tolerance = 1e-10)
})

test_that("os resíduos saem como série múltipla, iguais aos do VAR", {
  res <- tr_series_residuals_mv(var2)
  expect_true(stats::is.mts(res))
  expect_equal(colnames(res), colnames(Canada))
  expect_equal(as.numeric(res), as.numeric(stats::residuals(var2$ajuste)), tolerance = 1e-12)
  expect_equal(nrow(res), nrow(Canada) - 2L)
  expect_error(tr_series_residuals_mv(Canada), class = "tr_series_error_not_var")
})
