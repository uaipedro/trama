# GLM binomial negativa (MASS::glm.nb): contagem superdispersa com theta de
# máxima verossimilhança.
#
# Oráculos: (1) o próprio MASS::glm.nb, implementação de referência de
# Venables e Ripley (2002, sec. 7.4), no exemplo do livro (quine, Days ~ .^4);
# (2) máxima verossimilhança direta da NB2 por optim()
# sobre stats::dnbinom, independente do algoritmo alternado do glm.nb.

quine_df <- function() as.data.frame(MASS::quine)

test_that("binomial negativa bate com MASS::glm.nb no quine (Venables e Ripley 2002, sec. 7.4)", {
  d <- quine_df()
  g <- tr_models_glm(d, formula = "Days ~ (Eth + Sex + Age + Lrn)^4", familia = "binomial negativa")
  ref <- MASS::glm.nb(Days ~ .^4, data = d)  # a fórmula do livro
  expect_s3_class(g$ajuste, "negbin")
  expect_equal(unname(stats::coef(g$ajuste)), unname(stats::coef(ref)), tolerance = 1e-8)
  expect_equal(g$ajuste$theta, ref$theta, tolerance = 1e-8)
  expect_equal(g$ajuste$SE.theta, ref$SE.theta, tolerance = 1e-8)
  expect_equal(as.numeric(stats::logLik(g$ajuste)), as.numeric(stats::logLik(ref)), tolerance = 1e-8)
  expect_match(g$rotulo, "binomial negativa", fixed = TRUE)
  cf <- tr_models_coefficients(g)$tabela
  expect_equal(cf$erro_padrao, unname(sqrt(diag(stats::vcov(ref)))), tolerance = 1e-8)
})

test_that("theta e coeficientes batem com a máxima verossimilhança direta da NB2", {
  d <- quine_df()
  g <- tr_models_glm(d, resposta = "Days", preditores = c("Eth", "Sex", "Age", "Lrn"),
                     familia = "binomial negativa")
  X <- stats::model.matrix(~ Eth + Sex + Age + Lrn, d)
  nll <- function(p) {
    -sum(stats::dnbinom(d$Days, mu = exp(drop(X %*% p[-1])), size = exp(p[[1]]), log = TRUE))
  }
  ini <- c(0, stats::coef(stats::glm(Days ~ Eth + Sex + Age + Lrn, family = stats::poisson(), data = d)))
  o <- stats::optim(ini, nll, method = "BFGS", control = list(reltol = 1e-14, maxit = 1000))
  # Tolerância 1e-4 relativa: a do otimizador numérico, não a do glm.nb.
  expect_equal(g$ajuste$theta, exp(o$par[[1]]), tolerance = 1e-4)
  expect_equal(unname(stats::coef(g$ajuste)), unname(o$par[-1]), tolerance = 1e-4)
  expect_equal(as.numeric(stats::logLik(g$ajuste)), -o$value, tolerance = 1e-8)
})

test_that("models/compare: razão de verossimilhança com theta reestimado, como o anova.negbin", {
  d <- quine_df()
  m1 <- tr_models_glm(d, formula = "Days ~ Eth + Sex + Age", familia = "binomial negativa")
  m2 <- tr_models_glm(d, formula = "Days ~ Eth + Sex + Age + Lrn", familia = "binomial negativa")
  r <- tr_models_compare(m2, m1)
  a <- stats::anova(MASS::glm.nb(Days ~ Eth + Sex + Age, data = d),
                    MASS::glm.nb(Days ~ Eth + Sex + Age + Lrn, data = d))
  expect_equal(r$p_valor, a$`Pr(Chi)`[[2]], tolerance = 1e-8)
  x2 <- 2 * (as.numeric(stats::logLik(m2$ajuste)) - as.numeric(stats::logLik(m1$ajuste)))
  expect_equal(r$estatistica, x2, tolerance = 1e-8)
  # Família diferente (Poisson × NB) não se compara.
  mp <- tr_models_glm(d, formula = "Days ~ Eth + Sex + Age", familia = "poisson")
  expect_error(tr_models_compare(m2, mp), class = "tr_models_error_not_nested")
})

test_that("binomial negativa recusa contagem negativa; leitores aceitam o ajuste", {
  d <- quine_df()
  d$neg <- d$Days - 10
  expect_error(tr_models_glm(d, formula = "neg ~ Eth", familia = "binomial negativa"),
               class = "tr_models_error_bad_option")
  g <- tr_models_glm(d, formula = "Days ~ Eth + Age", familia = "binomial negativa")
  q <- tr_models_anova_table(g, tipo_sq = "II")
  ref <- car::Anova(MASS::glm.nb(Days ~ Eth + Age, data = d), type = 2)
  expect_equal(q$tabela$p_valor, unname(ref$`Pr(>Chisq)`), tolerance = 1e-8)
  q3 <- tr_models_anova_table(g, tipo_sq = "III")
  expect_true(all(is.finite(q3$tabela$p_valor)))
  p <- tr_models_predict(g)
  expect_equal(p$previsto, unname(stats::fitted(g$ajuste)), tolerance = 1e-10)
  expect_true(all(is.finite(tr_models_predict(g, validacao = "cruzada")$previsto)))
  expect_s3_class(tr_models_fit_stats(g), "data.frame")
})
