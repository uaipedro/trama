test_that("bootstrap de coeficientes usa o mesmo oráculo boot", {
  skip_if_not_installed("boot")
  d <- datasets::cars
  fit <- tr_models_lm(d, resposta = "dist", preditores = "speed")
  got <- tr_models_bootstrap(fit, repeticoes = 1999, confianca = .95, semente = 314)
  expect_equal(nrow(got$distribuicao), 3998L)
  expect_true(all(is.finite(got$tabela$erro_padrao)))
  expect_true(all(got$tabela$descartadas >= 0))
  expect_equal(got$tabela$estimativa, unname(stats::coef(fit$ajuste)), tolerance = 1e-12)
  expect_true(inherits(got$out, "ggplot"))
  X <- stats::model.matrix(fit$ajuste); y <- d$dist
  stat <- function(data, i) stats::lm.fit(X[i, , drop = FALSE], y[i])$coefficients
  ref <- trama.models:::.tr_models_com_semente(314, boot::boot(seq_len(nrow(d)), stat, R = 1999))
  for (j in seq_along(stats::coef(fit$ajuste))) {
    ci <- boot::boot.ci(ref, conf = .95, type = c("perc", "bca"), index = j)
    expect_equal(got$tabela$li_perc[j], ci$percent[4], tolerance = 1e-10)
    expect_equal(got$tabela$ls_perc[j], ci$percent[5], tolerance = 1e-10)
    expect_equal(got$tabela$li_bca[j], ci$bca[4], tolerance = 1e-10)
    expect_equal(got$tabela$ls_bca[j], ci$bca[5], tolerance = 1e-10)
  }
})
