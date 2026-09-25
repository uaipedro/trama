# Mínimos quadrados generalizados (nlme::gls): correlação no erro e variância
# por grupo, para medidas repetidas sem esfericidade.

ovario <- function() as.data.frame(nlme::Ovary) |> transform(Mare = factor(as.character(Mare)),
                                                            s = sin(2 * pi * Time), c = cos(2 * pi * Time))

test_that("gls AR(1) reproduz o nlme no Ovary (Pinheiro & Bates 2000)", {
  d <- ovario()
  g <- tr_models_gls(d, formula = "follicles ~ s + c", correlacao = "ar1", grupo = "Mare")
  ref <- nlme::gls(follicles ~ s + c, data = d, correlation = nlme::corAR1(form = ~ 1 | Mare))
  expect_equal(g$classe, "gls")
  expect_equal(unname(stats::coef(g$ajuste)), unname(stats::coef(ref)), tolerance = 1e-6)
  # Saída do nlme 3.1 impressa (REML): phi 0,7532, logLik -780,7273.
  expect_equal(round(unname(coef(g$ajuste$modelStruct$corStruct, unconstrained = FALSE)), 4), 0.7532)
  expect_equal(round(as.numeric(stats::logLik(g$ajuste)), 4), -780.7273)
  cf <- tr_models_coefficients(g)$tabela
  expect_equal(cf$erro_padrao, unname(summary(ref)$tTable[, "Std.Error"]), tolerance = 1e-6)
  expect_equal(cf$p_valor, unname(summary(ref)$tTable[, "p-value"]), tolerance = 1e-6)
  q <- tr_models_anova_table(g)$tabela
  a <- stats::anova(ref)
  expect_equal(q$p_valor, unname(a$`p-value`[-1]), tolerance = 1e-8)
})

test_that("gls simetria composta = misto de intercepto aleatório (correlação positiva)", {
  d <- ovario()
  g <- tr_models_gls(d, formula = "follicles ~ s + c", correlacao = "simetria_composta", grupo = "Mare")
  ref <- nlme::gls(follicles ~ s + c, data = d, correlation = nlme::corCompSymm(form = ~ 1 | Mare))
  expect_equal(as.numeric(stats::logLik(g$ajuste)), as.numeric(stats::logLik(ref)), tolerance = 1e-6)
  l <- nlme::lme(follicles ~ s + c, random = ~ 1 | Mare, data = d)
  expect_equal(unname(stats::coef(g$ajuste)), unname(nlme::fixef(l)), tolerance = 1e-4)
  expect_equal(as.numeric(stats::logLik(g$ajuste)), as.numeric(stats::logLik(l)), tolerance = 1e-4)
})

test_that("gls não estruturada + variância por idade reproduz o nlme no Orthodont; compara por RV", {
  o <- as.data.frame(nlme::Orthodont) |> transform(Subject = factor(as.character(Subject)), idade = factor(age))
  g <- tr_models_gls(o, formula = "distance ~ Sex * age", correlacao = "nao_estruturada", grupo = "Subject",
                     variancia_por = "idade")
  ref <- nlme::gls(distance ~ Sex * age, data = o, correlation = nlme::corSymm(form = ~ 1 | Subject),
                   weights = nlme::varIdent(form = ~ 1 | idade))
  expect_equal(as.numeric(stats::logLik(g$ajuste)), as.numeric(stats::logLik(ref)), tolerance = 1e-5)
  expect_equal(unname(stats::coef(g$ajuste)), unname(stats::coef(ref)), tolerance = 1e-5)
  g0 <- tr_models_gls(o, formula = "distance ~ Sex * age", correlacao = "simetria_composta", grupo = "Subject")
  cmp <- tr_models_compare(g0, g)
  ref0 <- nlme::gls(distance ~ Sex * age, data = o, correlation = nlme::corCompSymm(form = ~ 1 | Subject))
  expect_equal(cmp$p_valor, stats::anova(ref0, ref)$`p-value`[[2]], tolerance = 1e-6)
  expect_true(is.finite(tr_models_fit_stats(g)$aic))
  expect_s3_class(tr_models_emmeans(g0, "Sex"), "tr_models_emm")
})

test_that("gls recusa o que não fecha", {
  d <- ovario()
  expect_error(tr_models_gls(d, formula = "follicles ~ s", correlacao = "ar1"), class = "tr_models_error_bad_option")
  expect_error(tr_models_gls(d, formula = "follicles ~ s", correlacao = "xx", grupo = "Mare"),
               class = "tr_models_error_bad_option")
})
