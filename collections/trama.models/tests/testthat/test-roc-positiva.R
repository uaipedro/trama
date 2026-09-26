# A ROC da `ml` (com as correções da main: positiva deduzida do nome da coluna
# e AUC = Mann-Whitney) é a `models/roc` desde a Fase 4. Testes da main
# portados para o modo tabela daqui; `roc_()` devolve a AUC da curva.
roc_ <- function(d, resposta, probabilidade, positiva = "") {
  p <- tr_models_roc(dados = d, resposta = resposta, probabilidade = probabilidade, positiva = positiva)
  unique(p$layers[[2]]$data$auc)
}

test_that("ROC deduz a classe positiva do nome da coluna de probabilidade", {
  # Regressão: com `positiva` vazia, a segunda classe na ordem das linhas era
  # "nao" e a curva saía espelhada (AUC 0 em vez de 1).
  d <- tibble::tibble(y = c("sim", "nao", "sim", "nao"), .prob_sim = c(.8, .1, .7, .4))
  expect_equal(unique(roc_(d, "y", ".prob_sim")), 1)
  d2 <- tibble::tibble(y = d$y, .prob_nao = 1 - d$.prob_sim)
  expect_equal(unique(roc_(d2, "y", ".prob_nao")), 1)
  # Sem `.prob_<classe>` reconhecível, a classe não é adivinhada: exige `positiva`.
  d3 <- tibble::tibble(y = d$y, escore = d$.prob_sim)
  expect_error(roc_(d3, "y", "escore"), class = "tr_models_error_positive_required")
  d4 <- tibble::tibble(y = factor(d$y, levels = c("sim", "nao")), escore = d$.prob_sim)
  expect_error(roc_(d4, "y", "escore"), class = "tr_models_error_positive_required")
  d5 <- tibble::tibble(y = d$y, .prob_talvez = d$.prob_sim)
  expect_error(roc_(d5, "y", ".prob_talvez"), class = "tr_models_error_positive_required")
  expect_equal(unique(roc_(d4, "y", "escore", positiva = "sim")), 1)
})

test_that("AUC coincide com a estatística de Mann-Whitney (Hanley & McNeil 1982)", {
  # AUC = U / (n1 n0), com empates valendo 1/2 (Hanley & McNeil 1982, eq. 1).
  y <- c("a", "b", "b", "a", "b", "a", "b", "b", "a", "a")
  p <- c(.2, .9, .5, .5, .7, .1, .3, .8, .6, .4)
  auc <- unique(roc_(data.frame(y = y, .prob_b = p), "y", ".prob_b"))
  u <- suppressWarnings(stats::wilcox.test(p[y == "b"], p[y == "a"], exact = FALSE))$statistic
  expect_equal(auc, unname(u) / (5 * 5), tolerance = 1e-12)
  expect_equal(auc, 0.82, tolerance = 1e-12)
})
