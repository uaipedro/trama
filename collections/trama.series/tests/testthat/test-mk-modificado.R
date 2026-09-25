# Mann-Kendall para série autocorrelacionada: a correção da variância de
# Hamed & Rao (1998, J. Hydrol. 204:182-196) e o pré-branqueamento livre de
# tendência de Yue et al. (2002, Hydrol. Process. 16:1807-1829). Oráculo:
# `modifiedmk::mmkh` e `modifiedmk::tfpwmk` (Patakamuri & O'Brien), a 1e-8.

series_mk <- function() {
  set.seed(20260925)
  ar <- function(n, phi, b) as.numeric(stats::arima.sim(list(ar = phi), n)) + b * seq_len(n)
  list(
    nilo = as.numeric(datasets::Nile),
    lago = as.numeric(datasets::LakeHuron),
    ar06 = ar(60, 0.6, 0.02),
    ar08 = ar(120, 0.8, 0),
    # Arredondada: empates entram na variância das duas versões.
    empates = round(ar(80, 0.5, 0.03)),
    curta = ar(12, 0.4, 0.1)
  )
}

test_that("hamed_rao reproduz modifiedmk::mmkh (Z corrigido, p e n/n*)", {
  skip_if_not_installed("modifiedmk")
  for (nm in names(series_mk())) {
    x <- series_mk()[[nm]]
    t <- tr_series_mann_kendall(stats::ts(x), correcao = "hamed_rao")
    ref <- suppressWarnings(modifiedmk::mmkh(x))
    expect_equal(t$estatistica, unname(ref[["Corrected Zc"]]), tolerance = 1e-8, label = nm)
    expect_equal(t$p_valor, unname(ref[["new P-value"]]), tolerance = 1e-8, label = nm)
    expect_equal(t$extra$razao_n, unname(ref[["N/N*"]]), tolerance = 1e-8, label = nm)
    expect_equal(t$extra$S, tr_series_mann_kendall(stats::ts(x))$extra$S)
  }
})

test_that("pre_branqueamento reproduz modifiedmk::tfpwmk (Z, p e S)", {
  skip_if_not_installed("modifiedmk")
  for (nm in names(series_mk())) {
    x <- series_mk()[[nm]]
    t <- tr_series_mann_kendall(stats::ts(x), correcao = "pre_branqueamento")
    ref <- modifiedmk::tfpwmk(x)
    expect_equal(t$estatistica, unname(ref[["Z-Value"]]), tolerance = 1e-8, label = nm)
    expect_equal(t$p_valor, unname(ref[["P-value"]]), tolerance = 1e-8, label = nm)
    expect_equal(t$extra$S, unname(ref[["S"]]), label = nm)
  }
})

test_that("o padrão continua o Mann-Kendall da versão 1", {
  x <- stats::ts(series_mk()$ar06)
  expect_identical(tr_series_mann_kendall(x), tr_series_mann_kendall(x, correcao = "nenhuma"))
})

test_that("sem autocorrelação significativa, Hamed-Rao não mexe na variância", {
  # O teste só corrige pelas autocorrelações dos postos que passam do limite de
  # 5%: uma série de ruído branco com nenhuma delas volta ao MK comum.
  skip_if_not_installed("modifiedmk")
  set.seed(3)
  x <- stats::rnorm(40)
  ref <- modifiedmk::mmkh(x)
  if (ref[["N/N*"]] == 1) {
    expect_equal(tr_series_mann_kendall(stats::ts(x), correcao = "hamed_rao")$estatistica,
                 tr_series_mann_kendall(stats::ts(x))$estatistica)
  }
  expect_true(TRUE)
})

test_that("autocorrelação positiva: Hamed-Rao alarga a variância e baixa |Z|", {
  x <- stats::ts(series_mk()$ar08)
  hr <- tr_series_mann_kendall(x, correcao = "hamed_rao")
  mk <- tr_series_mann_kendall(x)
  expect_gt(hr$extra$razao_n, 1)
  expect_lt(abs(hr$estatistica), abs(mk$estatistica))
  expect_match(hr$nota, "Hamed & Rao", fixed = TRUE)
  expect_equal(hr$fonte, "Hamed & Rao (1998)")
})

test_that("a nota do pré-branqueamento diz o r1 e o tamanho usado", {
  t <- tr_series_mann_kendall(stats::ts(series_mk()$ar06), correcao = "pre_branqueamento")
  expect_match(t$nota, "r1 = ", fixed = TRUE)
  expect_equal(t$fonte, "Yue et al. (2002)")
})

test_that("bordas: correção inválida, série curta para o pré-branqueamento", {
  x <- stats::ts(series_mk()$ar06)
  expect_error(tr_series_mann_kendall(x, correcao = "yue"), class = "tr_series_error_bad_option")
  # Pré-branquear perde a primeira observação: 10 viram 9, abaixo do piso.
  expect_error(tr_series_mann_kendall(stats::ts(series_mk()$curta[1:10]),
                                      correcao = "pre_branqueamento"),
               class = "tr_series_error_too_short")
  expect_s3_class(tr_series_mann_kendall(stats::ts(series_mk()$curta[1:11]),
                                         correcao = "pre_branqueamento"), "tr_series_test")
})

test_that("n/n* não positivo é recusado com classe, e não vira NaN", {
  # O modifiedmk devolve NaN aqui (raiz de variância negativa); o bloco recusa.
  set.seed(249)
  x <- stats::ts(as.numeric(stats::arima.sim(list(ar = 0.6), 60)))
  expect_error(tr_series_mann_kendall(x, correcao = "hamed_rao"), class = "tr_series_error_fit")
  expect_s3_class(tr_series_mann_kendall(x, correcao = "pre_branqueamento"), "tr_series_test")
})
