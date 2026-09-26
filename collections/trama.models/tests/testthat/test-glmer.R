# Misto generalizado: o ajuste é o do glmer direto, e cada leitor que se
# aplica a ele devolve o que o lme4/car/emmeans devolvem.

cbpp_fit <- function(f = "cbind(incidence, size - incidence) ~ period + (1 | herd)") {
  tr_models_glmer(tibble::as_tibble(lme4::cbpp), formula = f, familia = "binomial")
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

test_that("Poisson sobredispersa: razão de Pearson nas medidas e nota nos coeficientes", {
  set.seed(9)
  d <- data.frame(bloco = rep(paste0("B", 1:8), each = 20), trat = rep(c("A", "B"), 80))
  mu <- exp(2 + 0.3 * (d$trat == "B"))
  d$n <- stats::rnbinom(nrow(d), mu = mu, size = 1.5)
  g <- tr_models_glmer(d, resposta = "n", fixos = "trat", grupo = "bloco", familia = "poisson")
  r <- sum(stats::residuals(g$ajuste, type = "pearson")^2) / stats::df.residual(g$ajuste)
  expect_equal(tr_models_fit_stats(g)$dispersao_pearson, r)
  expect_gt(r, 1.5)
  expect_match(tr_models_coefficients(g)$nota, "sobredispersão", fixed = TRUE)
  # Sem excesso, sem nota.
  d$m <- stats::rpois(nrow(d), mu)
  g2 <- tr_models_glmer(d, resposta = "m", fixos = "trat", grupo = "bloco", familia = "poisson")
  expect_false(grepl("sobredispersão", tr_models_coefficients(g2)$nota))
})

# Oráculo de teoria (Breslow & Clayton 1993; Bates et al. 2015): com um
# intercepto aleatório por rebanho, a log-verossimilhança do glmer (Laplace,
# nAGQ = 1) é, grupo a grupo, h(b̂) + ½log(2π) − ½log(−h''(b̂)), com
# h(b) = Σ log Bin(y | logit⁻¹(xβ + b)) + log N(b; 0, σ²). Refeita à mão nos
# parâmetros do ajuste; e maximizada à mão (optim) devolve os mesmos β e σ.
test_that("glmer: log-verossimilhança de Laplace refeita à mão, e o máximo dela", {
  g <- cbpp_fit()
  d <- as.data.frame(tibble::as_tibble(lme4::cbpp))
  X <- stats::model.matrix(~ period, d)
  laplace <- function(beta, sigma) {
    eta0 <- as.vector(X %*% beta)
    sum(vapply(split(seq_len(nrow(d)), d$herd), function(i) {
      h <- function(b) sum(stats::dbinom(d$incidence[i], d$size[i], stats::plogis(eta0[i] + b), log = TRUE)) +
        stats::dnorm(b, 0, sigma, log = TRUE)
      bh <- stats::optimize(h, c(-10, 10), maximum = TRUE, tol = 1e-12)$maximum
      p <- stats::plogis(eta0[i] + bh)
      h2 <- -sum(d$size[i] * p * (1 - p)) - 1 / sigma^2
      h(bh) + 0.5 * log(2 * pi) - 0.5 * log(-h2)
    }, 0))
  }
  beta <- lme4::fixef(g$ajuste); sigma <- sqrt(unlist(lme4::VarCorr(g$ajuste)))
  # 1e-5 relativo: o modo condicional do lme4 sai do PIRLS com a tolerância dele.
  expect_equal(laplace(beta, sigma), as.numeric(stats::logLik(g$ajuste)), tolerance = 1e-5)
  opt <- stats::optim(c(beta, log(sigma)), function(p) -laplace(p[1:4], exp(p[5])),
                      method = "BFGS", control = list(reltol = 1e-12))
  expect_equal(unname(opt$par[1:4]), unname(beta), tolerance = 1e-3)
  expect_equal(unname(exp(opt$par[5])), unname(sigma), tolerance = 1e-3)
})

# ---- da main (rigor): gm2, nivel_obs, tipo III, avisos na nota, dispersão ----

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
