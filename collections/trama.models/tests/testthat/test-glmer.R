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

test_that("Poisson sobredispersa: razão de Pearson nas medidas e nota nos coeficientes", {
  set.seed(9)
  d <- data.frame(bloco = rep(paste0("B", 1:8), each = 20), trat = rep(c("A", "B"), 80))
  mu <- exp(2 + 0.3 * (d$trat == "B"))
  d$n <- stats::rnbinom(nrow(d), mu = mu, size = 1.5)
  g <- tr_models_glmer(d, resposta = "n", fixos = "trat", grupo = "bloco", familia = "poisson")
  r <- sum(stats::residuals(g$ajuste, type = "pearson")^2) / stats::df.residual(g$ajuste)
  expect_equal(tr_models_fit_stats(g)$razao_dispersao, r)
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
  d <- as.data.frame(ex("cbpp"))
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
