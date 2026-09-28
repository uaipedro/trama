test_that("models/permutation reproduz F por permutação e limita a permutação a estratos", {
  dados <- data.frame(y = c(2, 3, 4, 5, 8, 9, 10, 11), g = factor(rep(c("a", "b"), each = 4)))
  fit <- tr_models_lm(dados, formula = "y ~ g")
  B <- 299L; seed <- 41L
  got <- tr_models_permutation(fit, termo = "g", reamostras = B, .seed = seed)

  set.seed(seed)
  fobs <- unname(stats::anova(fit$ajuste)$`F value`[1])
  fhand <- replicate(B, {
    d <- dados
    d$y <- sample(d$y)
    unname(stats::anova(stats::lm(y ~ g, d))$`F value`[1])
  })
  expect_equal(got$distribuicao$valor, fhand, tolerance = 1e-12)
  expect_equal(got$tabela$p_permutacao, (sum(fhand >= fobs - 1e-8) + 1) / (B + 1))
  expect_equal(got$tabela$p_teorico, stats::anova(fit$ajuste)$`Pr(>F)`[1])

  blocks <- data.frame(y = c(1, 4, 7, 10, 2, 5, 8, 12),
                       g = factor(rep(c("a", "b"), each = 4)),
                       bloco = factor(rep(1:4, 2)))
  fblock <- tr_models_lm(blocks, formula = "y ~ g + bloco")
  res <- tr_models_permutation(fblock, termo = "g", grupo = "bloco", reamostras = 20, .seed = seed)
  set.seed(seed)
  hblock <- replicate(20, {
    yp <- blocks$y
    for (i in split(seq_len(nrow(blocks)), blocks$bloco)) yp[i] <- sample(yp[i])
    d <- blocks; d$y <- yp
    unname(stats::anova(stats::lm(y ~ g + bloco, d))$`F value`[1])
  })
  expect_equal(res$distribuicao$valor, hblock, tolerance = 1e-12)
  expect_error(tr_models_permutation(fit, termo = "ausente", reamostras = 10), "não está no quadro")
  expect_error(tr_models_permutation(tr_models_glm(dados, formula = "y ~ g"), reamostras = 10), "apenas lm")
})

test_that("models/permutation concorda com o oráculo Monte Carlo publicado da coin", {
  skip_if_not_installed("coin")
  dados <- data.frame(y = c(1.2, 2.1, 1.7, 2.4, 3.1, 3.8, 4.2, 3.4),
                      g = factor(rep(c("a", "b"), each = 4)))
  fit <- tr_models_lm(dados, formula = "y ~ g")
  B <- 1999L
  got <- tr_models_permutation(fit, termo = "g", reamostras = B, .seed = 2025L)$tabela$p_permutacao
  set.seed(2025L)
  ref <- coin::oneway_test(y ~ g, data = dados,
                           distribution = coin::approximate(nresample = B))
  p_coin <- as.numeric(coin::pvalue(ref))
  se <- sqrt(p_coin * (1 - p_coin) / (B + 1))
  expect_lte(abs(got - p_coin), 4 * se + 1 / (B + 1))
})

test_that("models/permutation com k grupos concorda com a coin (PlantGrowth)", {
  skip_if_not_installed("coin")
  # No DIC, a estatística quadrática k-amostras da coin é função monótona do F
  # (Hothorn et al. 2008), então os dois p estimam o mesmo p de permutação.
  fit <- tr_models_lm(datasets::PlantGrowth, formula = "weight ~ group")
  B <- 9999L
  got <- tr_models_permutation(fit, termo = "group", reamostras = B, .seed = 7L)
  set.seed(7L)
  ref <- coin::oneway_test(weight ~ group, data = datasets::PlantGrowth,
                           distribution = coin::approximate(nresample = B))
  p_coin <- as.numeric(coin::pvalue(ref))
  expect_gt(p_coin, 0.005)
  se <- sqrt(p_coin * (1 - p_coin) / B)
  expect_lte(abs(got$tabela$p_permutacao - p_coin), 4 * sqrt(2) * se)
  expect_named(got$distribuicao, c("quantidade", "reamostra", "valor"))
  expect_equal(got$tabela$p_teorico, stats::anova(fit$ajuste)$`Pr(>F)`[1])
})

test_that("models/permutation recusa Residuals e NA em 'Dentro de'", {
  d <- data.frame(y = c(1, 4, 7, 10, 2, 5, 8, 12), g = factor(rep(c("a", "b"), each = 4)),
                  bloco = factor(c(1, 2, 3, NA, 1, 2, 3, 4)))
  fit <- tr_models_lm(d[c("y", "g")], formula = "y ~ g")
  expect_error(tr_models_permutation(fit, termo = "Residuals", reamostras = 99L), "não está no quadro")
  fit2 <- tr_models_lm(d, formula = "y ~ g")
  expect_error(tr_models_permutation(fit2, grupo = "bloco", reamostras = 99L), "tem NA")
})
