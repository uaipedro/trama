# Curva precisão-revocação e precisão média (veio da `ml/pr_curve` na 9.2;
# as contas e os oráculos são os de lá).

d_pr <- function() data.frame(y = c("sim", "nao", "sim", "nao", "sim"),
                              .prob_sim = c(.9, .8, .7, .6, .2))

test_that("exemplo à mão: AP e área de Davis & Goadrich", {
  z <- tr_models_pr_curve(dados = d_pr(), resposta = "y", probabilidade = ".prob_sim")$data
  # Cortes .9 .8 .7 .6 .2 -> (R, P) = (1/3, 1), (1/3, 1/2), (2/3, 2/3), (2/3, 1/2), (1, 3/5).
  expect_equal(z$recall, c(1, 1, 2, 2, 3) / 3)
  expect_equal(z$precision, c(1, 1 / 2, 2 / 3, 1 / 2, 3 / 5))
  # AP = soma de dR * P = (1 + 2/3 + 3/5) / 3.
  expect_equal(z$ap[[1]], (1 + 2 / 3 + 3 / 5) / 3, tolerance = 1e-12)
  # Área interpolada: 1/3 + [1 - ln(3/2)]/3 + [1 - 2 ln(5/4)]/3 (integral de
  # (a + x)/(c + x) em cada degrau com dFP = 0 depois do dTP).
  expect_equal(z$area[[1]], (1 + 1 - log(3 / 2) + 1 - 2 * log(5 / 4)) / 3, tolerance = 1e-12)
  expect_equal(z$prevalencia[[1]], 0.6)
})

test_that("oráculos: yardstick (AP) e PRROC (área interpolada)", {
  skip_if_not_installed("yardstick")
  skip_if_not_installed("PRROC")
  set.seed(5)
  y <- rbinom(200, 1, .15)
  p <- round(plogis(-1.5 + 1.8 * y + rnorm(200)), 2)       # com empates
  d <- data.frame(y = ifelse(y == 1, "raro", "comum"), .prob_raro = p)
  z <- tr_models_pr_curve(dados = d, resposta = "y", probabilidade = ".prob_raro")$data
  ref_ap <- yardstick::average_precision(
    data.frame(t = factor(d$y, levels = c("raro", "comum")), p = p), t, p)$.estimate
  expect_equal(z$ap[[1]], ref_ap, tolerance = 1e-10)
  ref <- PRROC::pr.curve(scores.class0 = p[y == 1], scores.class1 = p[y == 0])
  expect_equal(z$area[[1]], ref$auc.integral, tolerance = 1e-8)
  e <- d_pr()
  r2 <- PRROC::pr.curve(scores.class0 = e$.prob_sim[e$y == "sim"], scores.class1 = e$.prob_sim[e$y == "nao"])
  expect_equal(tr_models_pr_curve(dados = e, resposta = "y", probabilidade = ".prob_sim")$data$area[[1]], r2$auc.integral, tolerance = 1e-8)
})

test_that("bordas: classe positiva, uma classe, empate total", {
  e <- d_pr()
  expect_error(tr_models_pr_curve(dados = data.frame(y = e$y, escore = e$.prob_sim), resposta = "y", probabilidade = "escore"),
               class = "tr_models_error_positive_required")
  expect_error(tr_models_pr_curve(dados = data.frame(y = rep("sim", 3), .prob_sim = c(.1, .2, .3)), resposta = "y", probabilidade = ".prob_sim"),
               class = "tr_models_error_one_level")
  z <- tr_models_pr_curve(dados = data.frame(y = e$y, .prob_sim = .5), resposta = "y", probabilidade = ".prob_sim")$data
  expect_equal(nrow(z), 1L)
  expect_equal(z$ap[[1]], 0.6)                                # empate total = prevalência
  expect_equal(z$area[[1]], 0.6)
})

test_that("modos da models/roc: modelo com validação, modelo + dados, e a migração da ml", {
  m <- tr_models_glm(ex("mtcars"), formula = "am ~ wt", familia = "binomial")
  # Resubstituição = a tabela do models/predict no treino: mesma AP.
  z <- tr_models_pr_curve(m, validacao = "resubstituição")$data
  tab <- tr_models_predict(m, validacao = "resubstituição")
  zt <- tr_models_pr_curve(dados = tab, resposta = "am", probabilidade = "prob_1")$data
  expect_equal(z$ap[[1]], zt$ap[[1]], tolerance = 1e-12)
  expect_equal(z$area[[1]], zt$area[[1]], tolerance = 1e-12)
  # Com dados novos, a positiva padrão é o segundo nível.
  zn <- tr_models_pr_curve(m, ex("mtcars")[1:20, ])$data
  expect_equal(zn$prevalencia[[1]], mean(ex("mtcars")$am[1:20] == 1))
  reg <- models_registry()
  doc <- list(nodes = list(p = list(type = "ml/pr_curve",
                                    params = list(alvo = "y", probabilidade = ".prob_sim"))), edges = list())
  mg <- trama::tr_doc_migrate(doc, reg)
  expect_equal(mg$nodes$p$type, "models/pr_curve")
  expect_equal(mg$nodes$p$params$resposta, "y")
  expect_equal(mg$nodes$p$params$probabilidade, "prob_sim")
})
