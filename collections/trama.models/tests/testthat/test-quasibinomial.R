# GLM quasibinomial: a binomial agregada com a dispersão estimada.

cbpp_df <- function() {
  skip_if_not_installed("lme4")
  d <- get(utils::data("cbpp", package = "lme4", envir = environment()))
  d$sadios <- d$size - d$incidence
  as.data.frame(d)
}

test_that("quasibinomial bate com stats::glm: coeficientes, EP com dispersão, quadro F", {
  d <- cbpp_df()
  g <- tr_models_glm(d, formula = "cbind(incidence, sadios) ~ period", familia = "quasibinomial")
  ref <- stats::glm(cbind(incidence, sadios) ~ period, family = stats::quasibinomial(), data = d)
  expect_equal(unname(stats::coef(g$ajuste)), unname(stats::coef(ref)), tolerance = 1e-10)
  # Dispersão = X² de Pearson / gl do resíduo, e EP = EP da binomial × raiz dela
  # (o summary.glm usa os resíduos de trabalho da última iteração do IRLS:
  # iguais a 1e-6 relativo, a tolerância do ajuste).
  phi <- sum(stats::residuals(ref, type = "pearson")^2) / ref$df.residual
  expect_equal(summary(g$ajuste)$dispersion, phi, tolerance = 1e-5)
  expect_equal(summary(g$ajuste)$dispersion, summary(ref)$dispersion, tolerance = 1e-12)
  bin <- stats::glm(cbind(incidence, sadios) ~ period, family = stats::binomial(), data = d)
  cf <- tr_models_coefficients(g)$tabela
  expect_equal(cf$erro_padrao, unname(sqrt(diag(stats::vcov(bin))) * sqrt(phi)), tolerance = 1e-5)
  expect_equal(cf$erro_padrao, unname(sqrt(diag(stats::vcov(ref)))), tolerance = 1e-10)
  # O quadro é F (dispersão estimada), como o anova(test = "F") do stats.
  q <- tr_models_anova_table(g)
  a <- stats::anova(ref, test = "F")
  expect_equal(q$coluna_estat, "F")
  expect_equal(q$tabela$p_valor[q$tabela$termo == "period"], a$`Pr(>F)`[[2]], tolerance = 1e-8)
  expect_match(g$rotulo, "quasibinomial", fixed = TRUE)
})

test_that("quasibinomial com resposta 0/1 e recusa de proporção fora de [0, 1]", {
  mt <- ex("mtcars")
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "quasibinomial")
  ref <- stats::glm(am ~ wt, family = stats::quasibinomial(), data = mt)
  expect_equal(unname(stats::coef(g$ajuste)), unname(stats::coef(ref)), tolerance = 1e-10)
  expect_error(tr_models_glm(mt, formula = "mpg ~ wt", familia = "quasibinomial"))
})
