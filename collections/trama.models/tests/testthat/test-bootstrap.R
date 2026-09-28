# Oráculo: `boot::boot` + `boot::boot.ci` (Canty e Ripley; Davison e Hinkley,
# 1997) com a mesma semente e a mesma estatística escrita à mão, sem o bloco.
com_semente <- function(seed, expr) trama.models:::.tr_models_com_semente(seed, expr)

confere_ic <- function(got, ref, conf = 0.95) {
  # O mesmo tratamento de empates do bloco (ver `.tr_models_boot_empates`).
  ref$t <- trama.models:::.tr_models_boot_empates(ref$t, ref$t0)
  for (j in seq_len(nrow(got))) {
    ci <- boot::boot.ci(ref, conf = conf, type = c("perc", "bca"), index = j)
    expect_equal(got$li_perc[j], ci$percent[4], tolerance = 1e-10)
    expect_equal(got$ls_perc[j], ci$percent[5], tolerance = 1e-10)
    expect_equal(got$li_bca[j], ci$bca[4], tolerance = 1e-10)
    expect_equal(got$ls_bca[j], ci$bca[5], tolerance = 1e-10)
    expect_equal(got$erro_padrao[j], stats::sd(ref$t[, j]), tolerance = 1e-10)
    expect_equal(got$vies[j], unname(mean(ref$t[, j]) - ref$t0[j]), tolerance = 1e-10)
  }
}

test_that("coeficientes: IC percentil e BCa iguais ao boot.ci", {
  skip_if_not_installed("boot")
  d <- datasets::cars
  fit <- tr_models_lm(d, resposta = "dist", preditores = "speed")
  got <- tr_models_bootstrap(fit, reamostras = 1999L, .seed = 314L)
  stat <- function(dd, i) stats::coef(stats::lm(dist ~ speed, data = dd[i, ]))
  ref <- com_semente(314L, boot::boot(d, stat, R = 1999L))
  confere_ic(got$tabela, ref)
  expect_equal(got$tabela$quantidade, c("(Intercept)", "speed"))
  expect_equal(got$tabela$mesmo_sinal[2], mean(ref$t[, 2] > 0))
  expect_named(got$distribuicao, c("quantidade", "reamostra", "valor"))
  expect_equal(nrow(got$distribuicao), 2L * 1999L)
  expect_equal(got$distribuicao$valor[got$distribuicao$quantidade == "speed"], ref$t[, 2])
})

test_that("médias e diferenças: estratificado por tratamento, igual às médias por grupo", {
  skip_if_not_installed("boot")
  d <- datasets::PlantGrowth
  fit <- tr_models_anova_dic(d, resposta = "weight", tratamento = "group")
  medias <- function(dd, i) as.numeric(tapply(dd$weight[i], dd$group[i], mean))
  ref <- com_semente(11L, boot::boot(d, medias, R = 1999L, strata = d$group))

  got <- tr_models_bootstrap(fit, quantidade = "médias", reamostras = 1999L, .seed = 11L)
  expect_equal(got$tabela$quantidade, levels(d$group))
  expect_equal(got$tabela$estimativa, as.numeric(tapply(d$weight, d$group, mean)), tolerance = 1e-12)
  confere_ic(got$tabela, ref)

  difs <- function(dd, i) { m <- medias(dd, i); c(m[2] - m[1], m[3] - m[1], m[3] - m[2]) }
  refd <- com_semente(11L, boot::boot(d, difs, R = 1999L, strata = d$group))
  gotd <- tr_models_bootstrap(fit, quantidade = "diferenças", reamostras = 1999L, .seed = 11L)
  expect_equal(gotd$tabela$quantidade, c("trt1 - ctrl", "trt2 - ctrl", "trt2 - trt1"))
  confere_ic(gotd$tabela, refd)
})

test_that("médias com covariável são as do emmeans no ajuste original", {
  skip_if_not_installed("emmeans")
  d <- datasets::warpbreaks
  d$x <- seq_len(nrow(d)) / 10
  fit <- tr_models_lm(d, formula = "breaks ~ tension + x")
  got <- tr_models_bootstrap(fit, quantidade = "médias", especs = "tension", reamostras = 199L, .seed = 1L)
  em <- summary(emmeans::emmeans(fit$ajuste, "tension"))
  expect_equal(got$tabela$estimativa, em$emmean, tolerance = 1e-10)
})

test_that("reamostra sem um nível é descartada e contada; o resto segue", {
  skip_if_not_installed("boot")
  d <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 20), g = factor(c(rep("a", 7), "b")))
  fit <- tr_models_lm(d, formula = "y ~ g")
  got <- tr_models_bootstrap(fit, reamostras = 499L, .seed = 3L)
  expect_gt(got$tabela$descartadas[1], 0L)
  expect_equal(got$tabela$reamostras[1] + got$tabela$descartadas[1], 499L)
  expect_true(all(is.finite(c(got$tabela$li_perc, got$tabela$ls_perc))))
  expect_true(all(is.na(got$tabela$li_bca)))  # jackknife sem o único "b" não estima
  expect_error(tr_models_bootstrap(tr_models_glm(d, formula = "y ~ g")), "aceita ajuste lm")
})
