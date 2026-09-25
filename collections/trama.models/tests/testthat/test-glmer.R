# Misto generalizado: o ajuste é o do glmer direto, e cada leitor que se
# aplica a ele devolve o que o lme4/car/emmeans devolvem.

cbpp_fit <- function(f = "cbind(incidence, size - incidence) ~ period + (1 | herd)") {
  tr_models_glmer(ex("cbpp"), formula = f, familia = "binomial")
}

test_that("o ajuste é o do lme4::glmer, e os coeficientes são os do summary", {
  g <- cbpp_fit()
  ref <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), data = lme4::cbpp,
                     family = stats::binomial)
  expect_s3_class(g, "tr_models_glmer")
  expect_equal(lme4::fixef(g$ajuste), lme4::fixef(ref), tolerance = 1e-6)
  co <- tr_models_coefficients(g)$tabela
  s <- stats::coef(summary(ref))
  expect_equal(co$erro_padrao, unname(s[, 2]), tolerance = 1e-5)
  expect_equal(co$p_valor, unname(s[, 4]), tolerance = 1e-5)
  ex_ <- tr_models_coefficients(g, exponenciar = TRUE)$tabela
  expect_equal(ex_$estimativa, exp(co$estimativa))
  expect_equal(tr_models_fit_stats(g)$aic, stats::AIC(ref), tolerance = 1e-6)
})

test_that("atalho de colunas, Poisson e recusas", {
  set.seed(5)
  d <- data.frame(bloco = rep(paste0("B", 1:6), each = 10), trat = rep(c("A", "B"), 30))
  d$n <- stats::rpois(60, exp(1 + 0.5 * (d$trat == "B") + stats::rnorm(6, sd = 0.3)[as.integer(factor(d$bloco))]))
  g <- tr_models_glmer(d, resposta = "n", fixos = "trat", grupo = "bloco", familia = "poisson")
  ref <- lme4::glmer(n ~ trat + (1 | bloco), data = d, family = stats::poisson)
  expect_equal(lme4::fixef(g$ajuste), lme4::fixef(ref), tolerance = 1e-6)
  expect_error(tr_models_glmer(d, formula = "n ~ trat", familia = "poisson"), class = "tr_models_error_bad_formula")
  d$neg <- -d$n
  expect_error(tr_models_glmer(d, formula = "neg ~ trat + (1 | bloco)", familia = "poisson"),
               class = "tr_models_error_bad_option")
})

test_that("quadro de Wald, médias, variâncias, comparação e previsão", {
  g <- cbpp_fit()
  q <- tr_models_anova_table(g, "II")$tabela
  ref <- car::Anova(g$ajuste, type = 2)
  expect_equal(q$qui2, ref$Chisq)
  expect_error(tr_models_anova_table(g), class = "tr_models_error_not_applicable")
  e <- tr_models_emmeans(g, "period")
  ref_e <- summary(emmeans::emmeans(g$ajuste, "period", type = "response"))
  expect_equal(e$tabela$media, ref_e$prob)
  expect_equal(tr_models_random_effects(g)$variancia, as.data.frame(lme4::VarCorr(g$ajuste))$vcov)
  expect_error(tr_models_random_test(g), class = "tr_models_error_not_applicable")
  expect_error(tr_models_shapiro_residuals(g), class = "tr_models_error_not_applicable")
  # Comparar com o nulo, também depois do RDS (a chamada guarda os dados).
  g0 <- cbpp_fit("cbind(incidence, size - incidence) ~ 1 + (1 | herd)")
  f <- tempfile(fileext = ".rds"); saveRDS(g, f)
  t <- tr_models_compare(readRDS(f), g0)
  ref_c <- stats::anova(g0$ajuste, g$ajuste)
  expect_equal(t$p_valor, ref_c$`Pr(>Chisq)`[[2]])
  p <- tr_models_predict(g)
  expect_equal(p$previsto, unname(stats::fitted(g$ajuste)))
  expect_s3_class(tr_models_plot_caterpillar(g), "ggplot")
})
