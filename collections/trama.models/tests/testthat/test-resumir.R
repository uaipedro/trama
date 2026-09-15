test_that("SQ tipo II e III batem com o car, e o III reajusta em soma zero", {
  wb <- ex("warpbreaks")
  wb <- wb[-c(1, 2, 30), ]  # desbalanceado, para os tipos diferirem
  m <- tr_models_anova_factorial(wb, "breaks", "wool, tension")
  q2 <- tr_models_anova_table(m, "II")$tabela
  ref2 <- car::Anova(stats::lm(breaks ~ wool * tension, data = wb), type = 2)
  expect_equal(q2$F[1:3], unname(ref2$`F value`[1:3]), tolerance = 1e-8)
  q3 <- tr_models_anova_table(m, "III")$tabela
  ref3 <- car::Anova(stats::lm(breaks ~ wool * tension, data = wb,
                               contrasts = list(wool = "contr.sum", tension = "contr.sum")), type = 3)
  expect_equal(q3$F[1:3], unname(ref3$`F value`[2:4]), tolerance = 1e-8)
  expect_false(isTRUE(all.equal(q2$F[[1]], q3$F[[1]])))
})

test_that("o quadro tipo I fecha: as SQ somam o Total, com n − 1 gl; II e III não têm Total", {
  m <- milho_dbc()
  q <- tr_models_anova_table(m)$tabela
  expect_equal(q$termo, c("bloco", "hibrido", "Resíduo", "Total"))
  expect_equal(sum(q$sq[1:3]), q$sq[[4]])
  expect_equal(q$gl[[4]], nrow(m$dados) - 1)
  expect_false("Total" %in% tr_models_anova_table(m, "II")$tabela$termo)
})

test_that("o quadro traz CV e média no rodapé, e as estrelas seguem o summary", {
  q <- tr_models_anova_table(milho_dbc())
  expect_match(q$rodape$CV, "%$")
  expect_equal(.tr_models_estrelas(c(0.0001, 0.005, 0.03, 0.07, 0.5, NA)), c("***", "**", "*", ".", "ns", ""))
  expect_s3_class(q, "tr_models_effects")
})

test_that("coeficientes batem com o summary e exponenciam no GLM", {
  mt <- ex("mtcars")
  l <- tr_models_lm(mt, formula = "mpg ~ wt")
  c1 <- tr_models_coefficients(l)$tabela
  s <- stats::coef(summary(stats::lm(mpg ~ wt, data = mt)))
  expect_equal(c1$p_valor, unname(s[, 4]))
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  ce <- tr_models_coefficients(g, exponenciar = TRUE)$tabela
  expect_equal(ce$estimativa, unname(exp(stats::coef(g$ajuste))))
  expect_error(tr_models_coefficients(l, exponenciar = TRUE), class = "tr_models_error_not_applicable")
})

test_that("medidas de ajuste têm sempre as mesmas colunas", {
  mt <- ex("mtcars")
  a <- tr_models_fit_stats(tr_models_lm(mt, formula = "mpg ~ wt"))
  b <- tr_models_fit_stats(tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)"))
  c <- tr_models_fit_stats(tr_models_glm(mt, formula = "am ~ wt", familia = "binomial"))
  expect_equal(names(a), names(b)); expect_equal(names(a), names(c))
  expect_equal(a$r2, summary(stats::lm(mpg ~ wt, data = mt))$r.squared)
  expect_true(b$r2_condicional > b$r2_marginal)
  expect_true(c$desvio_explicado > 0 && c$desvio_explicado < 1)
})

test_that("R² marginal e condicional do misto só com intercepto seguem a fórmula", {
  m <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)")
  aj <- m$ajuste
  vf <- stats::var(stats::fitted(stats::lm(Reaction ~ Days, data = lme4::sleepstudy)))
  vf <- stats::var(as.vector(lme4::getME(aj, "X") %*% lme4::fixef(aj)))
  va <- as.data.frame(lme4::VarCorr(aj))$vcov[[1]]
  ve <- stats::sigma(aj)^2
  r <- .tr_models_r2_misto(aj)
  expect_equal(r[["marginal"]], vf / (vf + va + ve))
  expect_equal(r[["condicional"]], (vf + va) / (vf + va + ve))
})

test_that("efeitos aleatórios e o teste deles", {
  m <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (Days | Subject)")
  v <- tr_models_random_effects(m)
  expect_equal(v$componente, c("(Intercept)", "Days", "corr((Intercept), Days)", "resíduo"))
  expect_equal(sum(v$proporcao, na.rm = TRUE), 1)
  r <- tr_models_random_test(m)$tabela
  expect_equal(r$gl[[2]], 2)
  expect_error(tr_models_random_effects(milho_dbc()), class = "tr_models_error_not_applicable")
})

test_that("resíduos ao lado da tabela, sem sobrescrever coluna existente", {
  m <- milho_dbc()
  r <- tr_models_residuals(m)
  expect_equal(r$residuo, unname(stats::residuals(m$ajuste)))
  d <- ex("milho_dbc"); d$residuo <- 1
  r2 <- tr_models_residuals(tr_models_anova_dbc(d, "producao", "hibrido", "bloco"))
  expect_true(all(c("residuo", "residuo_modelo") %in% names(r2)))
  expect_s3_class(tr_models_plot_diagnostics(m), "ggplot")
})

test_that("comparar modelos aninhados, e recusar os não aninhados", {
  mt <- ex("mtcars")
  m1 <- tr_models_lm(mt, formula = "mpg ~ wt")
  m2 <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  t <- tr_models_compare(m2, m1)
  expect_equal(t$p_valor, stats::anova(stats::lm(mpg ~ wt, mt), stats::lm(mpg ~ wt + hp, mt))$`Pr(>F)`[[2]])
  expect_match(t$h0, "hp", fixed = TRUE)
  m3 <- tr_models_lm(mt, formula = "mpg ~ qsec")
  expect_error(tr_models_compare(m1, m3), class = "tr_models_error_not_nested")
  mt2 <- mt; mt2$hp[[1]] <- NA
  expect_error(tr_models_compare(m1, tr_models_lm(mt2, formula = "mpg ~ wt + hp")),
               class = "tr_models_error_not_nested")
  s <- ex("sleepstudy")
  a <- tr_models_lmer(s, formula = "Reaction ~ Days + (1 | Subject)")
  b <- tr_models_lmer(s, formula = "Reaction ~ Days + (Days | Subject)")
  expect_equal(tr_models_compare(a, b)$teste, "Razão de verossimilhança")
})
