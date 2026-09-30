# tidy/glance/augment: só renomeiam colunas dos leitores do contrato. O oráculo
# é o próprio `broom` (mesma tabela para lm e glm), com tolerância de ponto
# flutuante; o que o `broom` não tem (mista, GLS) confere-se contra o leitor.

mt <- datasets::mtcars

test_that("tidy de lm e glm coincide com broom::tidy", {
  skip_if_not_installed("broom")
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  a <- tidy(m, conf.int = TRUE); b <- broom::tidy(stats::lm(mpg ~ wt + hp, mt), conf.int = TRUE)
  expect_equal(names(a), names(b))
  expect_equal(as.data.frame(a), as.data.frame(b), tolerance = 1e-8)

  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  gb <- broom::tidy(stats::glm(am ~ wt, binomial, mt), conf.int = TRUE, conf.level = 0.9)
  ga <- tidy(g, conf.int = TRUE, conf.level = 0.9)
  # Wald (confint.default) no padrão da trama, perfil no broom: confere só o ponto.
  expect_equal(ga[c("term", "estimate", "std.error", "statistic", "p.value")],
               gb[c("term", "estimate", "std.error", "statistic", "p.value")], tolerance = 1e-6)
  expect_true(all(c("conf.low", "conf.high") %in% names(ga)))
})

test_that("tidy sem conf.int não traz intervalo; exponentiate exige GLM", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt")
  expect_false(any(c("conf.low", "conf.high") %in% names(tidy(m))))
  expect_error(tidy(m, exponentiate = TRUE), class = "tr_models_error_not_applicable")
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  expect_equal(tidy(g, exponentiate = TRUE)$estimate, exp(tidy(g)$estimate), tolerance = 1e-10)
})

test_that("glance e augment coincidem com o broom no lm", {
  skip_if_not_installed("broom")
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp"); f <- stats::lm(mpg ~ wt + hp, mt)
  gl <- glance(m); gb <- broom::glance(f)
  for (k in c("nobs", "df.residual", "r.squared", "adj.r.squared", "sigma", "logLik", "AIC", "BIC")) {
    expect_equal(gl[[k]], gb[[k]], tolerance = 1e-8, label = k)
  }
  au <- augment(m); ab <- broom::augment(f)
  expect_equal(au$.fitted, ab$.fitted, tolerance = 1e-8)
  expect_equal(au$.resid, ab$.resid, tolerance = 1e-8)
  expect_equal(au$.std.resid, ab$.std.resid, tolerance = 1e-8)
})

test_that("augment com newdata: regressão e classificação", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt")
  a <- augment(m, newdata = mt[1:3, ])
  expect_equal(a$.fitted, unname(stats::predict(stats::lm(mpg ~ wt, mt), mt[1:3, ])), tolerance = 1e-10)
  expect_equal(nrow(a), 3L)
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  ag <- augment(g, newdata = mt[1:3, ])
  expect_true(all(c(".pred_class", ".prob_0", ".prob_1") %in% names(ag)))
  expect_error(augment(m, newdata = mt[1:3, "hp", drop = FALSE]), class = "tr_models_error_unknown_column")
})

test_that("modelos misto e sem método: mesma tabela do leitor, ou erro com classe", {
  skip_if_not_installed("lme4")
  mm <- tr_models_lmer(ex("milho_dbc"), formula = "producao ~ hibrido + (1 | bloco)")
  td <- tidy(mm)
  expect_equal(td$estimate, tr_models_coefs(mm)$tabela$estimativa)
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value") %in% names(td)))
  fake <- structure(list(), class = c("tr_fake", "tr_models_fit"))
  expect_error(tidy(fake), class = "tr_models_error_no_method")
  expect_error(glance(fake), class = "tr_models_error_no_method")
})

test_that("augment sem newdata renomeia por nome, mesmo com colisão ou sem residuo_padronizado", {
  # Dados que já têm `residuo`: o leitor põe sufixo, e o augment ainda acha.
  d <- mt; d$residuo <- 1
  m <- tr_models_lm(d, formula = "mpg ~ wt")
  au <- augment(m)
  expect_equal(au$.resid, unname(stats::residuals(stats::lm(mpg ~ wt, mt))), tolerance = 1e-10)
  expect_equal(au$residuo, rep(1, nrow(mt)))
  # Classe com só duas colunas (como a `ml`): a última coluna de dados não pode virar `.fitted`.
  fake <- structure(list(), class = c("tr_fake2", "tr_models_fit"))
  local_mocked_s3_method("tr_models_resid", "tr_fake2", function(x) {
    d <- mt[1:3, c("mpg", "wt")]; d$ajustado <- c(1, 2, 3); d$residuo <- c(.1, .2, .3); d
  })
  a <- augment(fake)
  expect_equal(names(a), c("mpg", "wt", ".fitted", ".resid"))
  expect_equal(a$wt, mt$wt[1:3])
})

test_that("augment com newdata recusa colisão de nomes; conf.level inválido cita conf.level", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt")
  nd <- mt[1:3, ]; nd$.fitted <- 0
  expect_error(augment(m, newdata = nd), ".fitted", class = "tr_models_error_bad_option")
  expect_error(tidy(m, conf.level = 2), "conf.level", class = "tr_models_error_bad_option")
})

test_that("tidy repassa escala ao leitor; glance e tidy de glm e lmer", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  expect_false(isTRUE(all.equal(tidy(m)$estimate, tidy(m, escala = "desvio padrão")$estimate)))
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  gl <- glance(g)
  expect_true(is.na(gl$r.squared))
  expect_equal(gl$AIC, stats::AIC(stats::glm(am ~ wt, binomial, mt)), tolerance = 1e-8)
  skip_if_not_installed("lme4")
  mm <- tr_models_lmer(ex("milho_dbc"), formula = "producao ~ hibrido + (1 | bloco)")
  expect_true(is.na(glance(mm)$df.residual))
})
