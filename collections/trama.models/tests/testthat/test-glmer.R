# GLM misto (lme4::glmer): binomial e Poisson, com efeito aleatório por
# observação opcional para a superdispersão.

cbpp_d <- function() {
  d <- get(utils::data("cbpp", package = "lme4", envir = environment()))
  d$sadios <- d$size - d$incidence
  as.data.frame(d)
}

test_that("glmer reproduz a saída do lme4::glmer no cbpp: fixos, variância do rebanho, AIC", {
  d <- cbpp_d()
  g <- tr_models_glmer(d, formula = "cbind(incidence, sadios) ~ period + (1 | herd)", familia = "binomial")
  expect_equal(g$classe, "glmer")
  # Reproduz a saída do lme4::glmer no exemplo de ?lme4::glmer (gm1), com 4
  # decimais — saída do software, não valor publicado em artigo.
  expect_equal(round(unname(lme4::fixef(g$ajuste)), 4), c(-1.3983, -0.9919, -1.1282, -1.5797))
  re <- tr_models_random_effects(g)
  expect_equal(round(re$variancia[re$grupo == "herd"], 4), 0.4123)
  expect_equal(round(stats::AIC(g$ajuste), 1), 194.1)
  # E o próprio lme4, na precisão do otimizador.
  ref <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), family = stats::binomial(), data = d)
  expect_equal(unname(lme4::fixef(g$ajuste)), unname(lme4::fixef(ref)), tolerance = 1e-6)
  cf <- tr_models_coefficients(g)$tabela
  s <- stats::coef(summary(ref))
  expect_equal(cf$erro_padrao, unname(s[, "Std. Error"]), tolerance = 1e-5)
  expect_equal(cf$p_valor, unname(s[, "Pr(>|z|)"]), tolerance = 1e-4)
  expect_true(all(is.na(re$proporcao)))
  fs <- tr_models_fit_stats(g)
  expect_equal(fs$log_verossimilhanca, as.numeric(stats::logLik(ref)), tolerance = 1e-6)
  # Quadro: qui-quadrado de Wald do car::Anova.
  q <- tr_models_anova_table(g, "II")
  expect_equal(q$tabela$p_valor[[1]], car::Anova(ref, type = "II")$`Pr(>Chisq)`[[1]], tolerance = 1e-4)
})

test_that("glmer com efeito por observação reproduz o gm2 do lme4", {
  d <- cbpp_d()
  g <- tr_models_glmer(d, formula = "cbind(incidence, sadios) ~ period + (1 | herd)", familia = "binomial",
                       nivel_obs = TRUE)
  d$obs <- factor(seq_len(nrow(d)))
  ref <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd) + (1 | obs),
                     family = stats::binomial(), data = d)
  expect_equal(unname(lme4::fixef(g$ajuste)), unname(lme4::fixef(ref)), tolerance = 1e-5)
  re <- tr_models_random_effects(g)
  expect_equal(re$desvio_padrao[re$grupo == ".obs"], 0.89107, tolerance = 1e-4)
  # Comparação: o efeito por observação melhora o ajuste? (RV)
  g1 <- tr_models_glmer(d, formula = "cbind(incidence, sadios) ~ period + (1 | herd)", familia = "binomial")
  cmp <- tr_models_compare(g1, g)
  expect_equal(cmp$p_valor, stats::anova(lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                                                     family = stats::binomial(), data = d), ref)$`Pr(>Chisq)`[[2]],
               tolerance = 1e-4)
})

test_that("glmer Poisson, atalho por colunas, e recusas", {
  gr <- get(utils::data("grouseticks", package = "lme4", envir = environment()))
  g <- tr_models_glmer(gr, resposta = "TICKS", fixos = "YEAR", grupo = "BROOD", familia = "poisson")
  ref <- lme4::glmer(TICKS ~ YEAR + (1 | BROOD), family = stats::poisson(), data = gr)
  expect_equal(unname(lme4::fixef(g$ajuste)), unname(lme4::fixef(ref)), tolerance = 1e-5)
  expect_error(tr_models_glmer(gr, formula = "TICKS ~ YEAR", familia = "poisson"), class = "tr_models_error_bad_formula")
  expect_error(tr_models_glmer(gr, formula = "TICKS ~ YEAR + (1 | BROOD)", familia = "gama"),
               class = "tr_models_error_bad_option")
  expect_error(tr_models_shapiro_residuals(g), class = "tr_models_error_not_applicable")
  expect_error(tr_models_duncan(g, "YEAR"), class = "tr_models_error_not_applicable")
  em <- tr_models_emmeans(g, "YEAR")
  expect_s3_class(em, "tr_models_emm")
})

test_that("glmer tipo III reajusta com contr.sum (efeito na média, não no nível de referência)", {
  gr <- as.data.frame(get(utils::data("grouseticks", package = "lme4", envir = environment())))
  gr$alt <- factor(ifelse(gr$HEIGHT > stats::median(gr$HEIGHT), "alta", "baixa"))
  g <- tr_models_glmer(gr, formula = "TICKS ~ YEAR * alt + (1 | BROOD)", familia = "poisson")
  q <- tr_models_anova_table(g, "III")$tabela
  # Oráculo: car::Anova(type = 3) no glmer ajustado com contr.sum nos fatores
  # fixos. Com o contraste de tratamento o YEAR sairia 50,52 e o alt 41,71.
  ref <- lme4::glmer(TICKS ~ YEAR * alt + (1 | BROOD), family = stats::poisson(), data = gr,
                     contrasts = list(YEAR = "contr.sum", alt = "contr.sum"))
  a <- car::Anova(ref, type = 3)
  expect_equal(q$termo, c("YEAR", "alt", "YEAR:alt"))
  expect_equal(q$qui2, unname(a$Chisq[-1]), tolerance = 1e-6)
  expect_equal(q$p_valor, unname(a$`Pr(>Chisq)`[-1]), tolerance = 1e-6)
  expect_equal(round(q$qui2, 1), c(80.9, 72.2, 6.4))
  # O tipo II não muda com o contraste e fica no ajuste original.
  q2 <- tr_models_anova_table(g, "II")$tabela
  expect_equal(q2$qui2, unname(car::Anova(g$ajuste, type = 2)$Chisq), tolerance = 1e-8)
})

test_that("glmer guarda os avisos do ajuste na nota e recusa respostas binomiais inválidas", {
  # Grupos idênticos: variância do grupo no limite (ajuste singular). O lme4
  # avisa "boundary (singular) fit"; o aviso tem de chegar ao card.
  d <- data.frame(g = factor(rep(1:5, each = 4)), y = rep(c(0, 1, 1, 0), 5))
  g <- tr_models_glmer(d, formula = "y ~ 1 + (1 | g)", familia = "binomial")
  expect_true(lme4::isSingular(g$ajuste))
  expect_match(g$nota, "singular")
  expect_equal(tr_models_glmer(cbpp_d(), formula = "cbind(incidence, sadios) ~ period + (1 | herd)")$nota, "")
  expect_match(tr_models_coefficients(g)$nota, "singular")
  expect_match(.tr_models_fit_preview(g)$nota, "singular")
  # Bernoulli + efeito por observação: não identificável.
  expect_error(tr_models_glmer(d, formula = "y ~ 1 + (1 | g)", familia = "binomial", nivel_obs = TRUE),
               class = "tr_models_error_bad_option")
  # Proporção numa coluna só, sem o total: recusa e aponta o cbind.
  cb <- cbpp_d(); cb$prop <- cb$incidence / cb$size
  expect_error(tr_models_glmer(cb, formula = "prop ~ period + (1 | herd)", familia = "binomial"),
               "cbind", class = "tr_models_error_bad_option")
  # Contagem de sucessos sem o total também.
  expect_error(tr_models_glmer(cb, formula = "incidence ~ period + (1 | herd)", familia = "binomial"),
               class = "tr_models_error_bad_option")
})

test_that("fit_stats dá X² de Pearson / gl e desvio / gl no glmer (superdispersão verificável)", {
  gr <- as.data.frame(get(utils::data("grouseticks", package = "lme4", envir = environment())))
  g <- tr_models_glmer(gr, formula = "TICKS ~ YEAR + (1 | BROOD)", familia = "poisson")
  fs <- tr_models_fit_stats(g)
  ref <- lme4::glmer(TICKS ~ YEAR + (1 | BROOD), family = stats::poisson(), data = gr)
  # Oráculo: sum(residuals(fit, "pearson")^2) / df.residual(fit) no próprio lme4.
  expect_equal(fs$dispersao_pearson, sum(stats::residuals(ref, "pearson")^2) / stats::df.residual(ref),
               tolerance = 1e-6)
  expect_equal(fs$desvio_por_gl, sum(stats::residuals(ref, "deviance")^2) / stats::df.residual(ref),
               tolerance = 1e-6)
  expect_equal(round(fs$dispersao_pearson, 3), 1.692)  # grouseticks é superdisperso
  # O efeito por observação absorve a superdispersão.
  go <- tr_models_glmer(gr, formula = "TICKS ~ YEAR + (1 | BROOD)", familia = "poisson", nivel_obs = TRUE)
  expect_lt(tr_models_fit_stats(go)$dispersao_pearson, fs$dispersao_pearson)
  # GLM Poisson: a razão clássica do desvio.
  gm <- tr_models_glm(datasets::InsectSprays, formula = "count ~ spray", familia = "poisson")
  r <- stats::glm(count ~ spray, family = stats::poisson(), data = datasets::InsectSprays)
  expect_equal(tr_models_fit_stats(gm)$desvio_por_gl, r$deviance / r$df.residual, tolerance = 1e-10)
  # Binomial 0/1: não mede superdispersão.
  d <- data.frame(g = factor(rep(1:6, each = 5)), y = rep(c(0, 1, 1, 0, 1), 6))
  expect_true(is.na(tr_models_fit_stats(tr_models_glmer(d, formula = "y ~ 1 + (1 | g)"))$dispersao_pearson))
})
