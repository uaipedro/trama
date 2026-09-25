# Oráculo: `MASS::boxcox` na mesma grade (tolerância 1e-10, ponto flutuante) e
# os dados de Box & Cox (1964), `boot::poisons`. Que os autores recomendam a
# recíproca (λ = −1) nesses dados é leitura a conferir no artigo; aqui só se
# exige que ela caia no IC de 95% calculado.

test_that("perfil igual ao do MASS::boxcox na mesma grade, e o IC nos pontos certos", {
  skip_if_not_installed("boot")
  d <- boot::poisons
  m <- trama.models::tr_models_anova_factorial(d, "time", "poison, treat")
  b <- tr_experiments_boxcox(m)
  mb <- MASS::boxcox(lm(time ~ poison * treat, data = boot::poisons), plotit = FALSE, interp = FALSE)
  expect_equal(b$perfil$lambda, mb$x, tolerance = 1e-12)
  expect_equal(b$perfil$log_verossimilhanca, mb$y, tolerance = 1e-10)
  expect_lte(abs(b$resumo$lambda_otimo - mb$x[which.max(mb$y)]), 0.05)
  expect_gte(max(b$perfil$log_verossimilhanca), max(mb$y) - 1e-12)
  expect_true(b$resumo$li < -1 && b$resumo$ls > -1)
  expect_equal(b$resumo$lambda_sugerido, -1)
  expect_false(b$resumo$um_no_intervalo)
  expect_s3_class(b$out, "ggplot")
})

test_that("resposta com zero é recusada", {
  d <- dados_gravacao(); d$taxa[[1]] <- 0
  m <- trama.models::tr_models_anova_dic(d, "taxa", "potencia")
  expect_error(tr_experiments_boxcox(m), class = "tr_experiments_error_not_applicable")
})

test_that("λ̂ na borda da grade é dito borda, sem sugestão", {
  set.seed(777)
  d <- expand.grid(t = factor(1:5), b = factor(1:6))
  d$y <- exp(1 + 0.3 * as.numeric(d$t) + 0.2 * as.numeric(d$b) + stats::rnorm(30, sd = 0.4))
  m <- trama.models::tr_models_anova_dbc(d, "y", "t", "b")
  livre <- tr_experiments_boxcox(m)$resumo
  expect_false(livre$na_borda)
  expect_lt(livre$lambda_otimo, 0.5)  # o ótimo de verdade fica abaixo de 0,5
  b <- tr_experiments_boxcox(m, lambda_min = 0.5, lambda_max = 2)$resumo
  expect_true(b$na_borda)
  expect_equal(b$lambda_otimo, 0.5)
  expect_true(is.na(b$lambda_sugerido))
  expect_match(b$nota, "borda inferior")
})
