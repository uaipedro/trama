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

# Medidas repetidas com interação tratamento × tempo (16 unidades, 4 ocasiões;
# gerado com AR(1) phi = 0,6 e arredondado a 3 casas).
medidas_trt <- function() {
  d <- expand.grid(t = 1:4, id = 1:16)
  d$trt <- factor(ifelse(d$id <= 8, "A", "B")); d$id <- factor(d$id)
  d$y <- c(7.077, 9.03, 9.032, 9.063, 13.737, 13.199, 13.367, 13.026, 10.869, 10.472, 11.226, 11.196,
           10.96, 11.217, 9.377, 11.544, 6.975, 8.448, 6.629, 7.656, 9.179, 9.33, 9.274, 9.903, 8.252,
           8.104, 8.057, 8.802, 10.974, 9.434, 10.335, 9.032, 9.687, 11.521, 12.354, 12.753, 12.289,
           10.886, 11.524, 12.459, 11.933, 13.311, 13.657, 15.689, 10.966, 13.088, 14.459, 13.564,
           10.064, 11.549, 11.952, 10.706, 9.787, 10.474, 11.745, 10.96, 11.514, 11.671, 11.788,
           13.001, 11.585, 11.489, 12.313, 13.66)
  d$tf <- factor(d$t)
  d
}

test_that("gls tipo III (marginal) reajusta com contr.sum", {
  d <- medidas_trt()
  g <- tr_models_gls(d, formula = "y ~ trt * tf", correlacao = "ar1", grupo = "id", tempo = "t")
  q <- tr_models_anova_table(g, "III")$tabela
  # Oráculo: anova(type = "marginal") do nlme com contr.sum nos dois fatores.
  # Com o contraste de tratamento o trt sairia F = 2,14 (p = 0,149): o efeito
  # do tratamento só na ocasião 1.
  dd <- d
  stats::contrasts(dd$trt) <- stats::contr.sum(2); stats::contrasts(dd$tf) <- stats::contr.sum(4)
  ref <- nlme::gls(y ~ trt * tf, data = dd, correlation = nlme::corAR1(form = ~ t | id))
  a <- stats::anova(ref, type = "marginal")
  expect_equal(q$F, unname(a$`F-value`[-1]), tolerance = 1e-6)
  expect_equal(q$p_valor, unname(a$`p-value`[-1]), tolerance = 1e-6)
  expect_equal(round(q$F, 4), c(8.6531, 2.3951, 1.8083))
  # O sequencial não depende do contraste.
  q1 <- tr_models_anova_table(g, "I")$tabela
  expect_equal(q1$F, unname(stats::anova(ref)$`F-value`[-1]), tolerance = 1e-6)
})

test_that("lmer tipo III (lmerTest) já não depende do contraste", {
  d <- medidas_trt()
  m <- tr_models_lmer(d, formula = "y ~ trt * tf + (1 | id)")
  q <- tr_models_anova_table(m, "III")$tabela
  ref <- lmerTest::lmer(y ~ trt * tf + (1 | id), data = d,
                        contrasts = list(trt = "contr.sum", tf = "contr.sum"))
  expect_equal(q$F, unname(stats::anova(ref, type = 3)$`F value`), tolerance = 1e-6)
  expect_equal(round(q$F, 4), c(8.6046, 5.7260, 4.3282))
})

test_that("gls: emmeans com gl de Satterthwaite explícitos, e a nota diz que o quadro usa n − p", {
  d <- medidas_trt()
  g <- tr_models_gls(d, formula = "y ~ trt", correlacao = "ar1", grupo = "id", tempo = "t")
  em <- tr_models_emmeans(g, "trt")
  ref <- summary(emmeans::emmeans(g$ajuste, "trt", mode = "satterthwaite"))
  expect_equal(em$tabela$gl, ref$df, tolerance = 1e-8)
  expect_equal(em$tabela$li, ref$lower.CL, tolerance = 1e-8)
  expect_match(em$nota, "Satterthwaite")
  # Os coeficientes ficam nos gl do nlme, n − p = 64 − 2 = 62: mais que os de
  # Satterthwaite com 16 sujeitos.
  expect_equal(g$ajuste$dims$N - g$ajuste$dims$p, 62)
  expect_true(all(em$tabela$gl < 62))
})
