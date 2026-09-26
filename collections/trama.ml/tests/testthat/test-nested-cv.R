# Validação cruzada aninhada (Varma & Simon 2006, BMC Bioinformatics 7:91).

test_that("aninhada: contabilidade dos folds e fold externo refeito à mão", {
  d <- tr_ml_example("iris_binaria")
  z <- tr_ml_nested_cv(d, "Species", tentativas = 3, folds_externos = 3, folds = 3, seed = 7)
  expect_equal(z$fold, c("1", "2", "3", "media"))
  expect_true(all(z$n_treino[1:3] + z$n_validacao[1:3] == nrow(d)))
  expect_equal(sum(z$n_validacao[1:3]), nrow(d))           # cada linha valida uma vez
  expect_equal(z$externa[[4]], mean(z$externa[1:3]))
  expect_equal(z$interna[[4]], mean(z$interna[1:3]))
  # Fold 2 refeito: mesmos folds externos (semente 7) e busca só no treino externo (semente 7 + 2).
  partes <- .tr_ml_with_seed(7L, .tr_ml_folds(d, "aleatoria", 3L, factor(d$Species)))
  t2 <- tr_ml_tune(d[partes[[2]]$treino, ], "Species", tentativas = 3, folds = 3, seed = 9L)
  v2 <- d[partes[[2]]$validacao, ]
  p2 <- .tr_ml_prever(t2$modelo, .tr_ml_novos_dados(t2$modelo, v2))$previsto
  expect_equal(z$externa[[2]], .tr_ml_tune_metric(v2$Species, p2, "classificacao", "macro_f1"))
  expect_equal(z$interna[[2]], t2$historico$media[[t2$melhor_tentativa]])
  expect_identical(z, tr_ml_nested_cv(d, "Species", tentativas = 3, folds_externos = 3, folds = 3, seed = 7))
  expect_error(tr_ml_nested_cv(d, "Species", folds_externos = 1), class = "tr_ml_error_bad_param")
})

test_that("aninhada por grupo: validação externa nunca compartilha grupo com o treino", {
  set.seed(2)
  d <- data.frame(y = rnorm(48), x = rnorm(48), lote = rep(1:8, each = 6))
  z <- tr_ml_nested_cv(d, "y", tentativas = 2, folds_externos = 4, folds = 3,
                       estrategia = "grupo", grupo = "lote", seed = 1)
  expect_true(all(z$n_validacao[1:4] == 12))                # 2 lotes inteiros por fold
})

test_that("em ruído puro, aninhada fica no acaso e a não aninhada é otimista", {
  skip_on_cran()
  skip_if_not_installed("e1071")
  # Simulação semeada: 12 réplicas, n = 40, 10 preditores de ruído, classes
  # equilibradas (acurácia do acaso = 0,5), SVM com 10 tentativas e 5 folds.
  # Medido: não aninhada 0,604, aninhada 0,519 (EP 0,017). Tolerância
  # declarada: |aninhada - 0,5| < 0,06 e otimismo > 0,05.
  sim <- function(r) {
    set.seed(r)
    d <- data.frame(y = factor(rep(c("a", "b"), 20)), matrix(rnorm(400), 40))
    nn <- tr_ml_tune(d, "y", modelo = "svm", metrica = "accuracy", tentativas = 10, folds = 5, seed = r)
    z <- tr_ml_nested_cv(d, "y", modelo = "svm", metrica = "accuracy", tentativas = 10,
                         folds_externos = 5, folds = 5, seed = r)
    c(nn$historico$media[[nn$melhor_tentativa]], z$externa[[6]])
  }
  r <- colMeans(do.call(rbind, lapply(1:12, sim)))
  expect_lt(abs(r[[2]] - 0.5), 0.06)
  expect_gt(r[[1]] - r[[2]], 0.05)
})
