test_that("contrastes nas médias: F conjunto bate com o emmeans e com a ANOVA", {
  dic <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  t <- tr_models_linear_hypothesis(dic, "ctrl vs trat: 2 -1 -1; trt1 vs trt2: trt1 - trt2", "group")
  # Dois contrastes ortogonais entre 3 níveis esgotam o tratamento: o F conjunto
  # É o F da ANOVA.
  q <- tr_models_anova_table(dic)$tabela
  expect_equal(t$estatistica, q$F[[1]], tolerance = 1e-8)
  expect_equal(t$gl, "2; 27")
  expect_equal(names(t$extra), c("ctrl vs trat", "trt1 vs trt2"))
  # Números e nomes dos níveis escrevem o mesmo contraste.
  a <- tr_models_linear_hypothesis(dic, "0 1 -1", "group")
  b <- tr_models_linear_hypothesis(dic, "trt1 - trt2", "group")
  expect_equal(a$p_valor, b$p_valor)
  ref <- summary(emmeans::contrast(emmeans::emmeans(dic$ajuste, "group"), list(x = c(0, 1, -1))))
  expect_equal(a$estatistica, ref$t.ratio^2, tolerance = 1e-8)
})

test_that("contrastes com nome não sintático, nota de quem não soma zero, parcela subdividida", {
  sp <- tr_models_anova_split_plot(ex("aveia"), "producao", "variedade", "nitrogenio", "bloco")
  t <- tr_models_linear_hypothesis(sp, "extremos: `0.6cwt` - `0.0cwt`", "nitrogenio")
  expect_true(t$p_valor < 1e-6)
  dic <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  expect_match(tr_models_linear_hypothesis(dic, "1 0 0", "group")$nota, "não somam zero", fixed = TRUE)
})

test_that("hipótese nos coeficientes bate com o car no lm, glm e misto", {
  mt <- ex("mtcars")
  l <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  t <- tr_models_linear_hypothesis(l, "wt = 0; hp = 0")
  ref <- car::linearHypothesis(l$ajuste, c("wt = 0", "hp = 0"))
  expect_equal(t$estatistica, ref$F[[2]])
  expect_equal(t$gl, "2; 29")
  g <- tr_models_glm(ex("InsectSprays"), "count", "spray")
  expect_equal(tr_models_linear_hypothesis(g, "sprayB = sprayF")$p_valor,
               car::linearHypothesis(g$ajuste, "sprayB = sprayF")$`Pr(>Chisq)`[[2]])
  s <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)")
  expect_equal(tr_models_linear_hypothesis(s, "Days = 0")$p_valor,
               tr_models_coefficients(s)$tabela$p_valor[[2]], tolerance = 1e-8)
})

test_that("recusas: dependentes, tamanho errado, nome desconhecido, fator numérico", {
  dic <- tr_models_anova_dic(ex("PlantGrowth"), "weight", "group")
  expect_error(tr_models_linear_hypothesis(dic, "1 -1 0; 2 -2 0", "group"), class = "tr_models_error_bad_option")
  expect_error(tr_models_linear_hypothesis(dic, "1 -1", "group"), class = "tr_models_error_bad_option")
  expect_error(tr_models_linear_hypothesis(dic, "ctrl - trt9", "group"), class = "tr_models_error_bad_option")
  expect_error(tr_models_linear_hypothesis(dic, "", "group"), class = "tr_models_error_blank_param")
  l <- tr_models_lm(ex("mtcars"), formula = "mpg ~ wt + cyl")
  err <- tryCatch(tr_models_linear_hypothesis(l, "peso = 0"), condition = identity)
  expect_s3_class(err, "tr_models_error_bad_option")
  expect_match(conditionMessage(err), "wt", fixed = TRUE)
  expect_error(tr_models_linear_hypothesis(l, "1 -1", "cyl"), class = "tr_models_error_not_applicable")
  expect_error(tr_models_linear_hypothesis(dic, "a: 1 -1 0; a: 0 1 -1", "group"), class = "tr_models_error_bad_option")
})
