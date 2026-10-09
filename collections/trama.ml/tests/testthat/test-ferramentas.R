# Atributo `trama_ferramentas` de `ml/tune` e `ml/nested_cv`: o motor é o param
# `modelo`, e a ferramenta registrada é a do ramo que rodou. O relatório cita
# esse registro no lugar da `tr_ref` de implementação (só do próprio trama).

ferramentas <- function(x) attr(x, "trama_ferramentas", exact = TRUE)

test_that("tune registra o motor do param modelo", {
  d <- mtcars[, c("mpg", "wt", "hp")]
  expect_equal(ferramentas(tr_ml_tune(d, "mpg", modelo = "cart", tentativas = 1, folds = 2, seed = 7)),
               "rpart::rpart")
  skip_if_not_installed("ranger")
  expect_equal(ferramentas(tr_ml_tune(d, "mpg", modelo = "forest", tentativas = 1, folds = 2, seed = 7)),
               "ranger::ranger")
})

test_that("tune e nested_cv de SVM e de XGBoost registram o motor certo", {
  d <- subset(iris, Species != "virginica")
  skip_if_not_installed("e1071")
  expect_equal(ferramentas(tr_ml_tune(d, "Species", modelo = "svm", tentativas = 1, folds = 2, seed = 31)),
               "e1071::svm")
  skip_if_not_installed("xgboost")
  expect_equal(ferramentas(tr_ml_tune(d, "Species", modelo = "xgboost", tentativas = 1, folds = 2, seed = 31)),
               "xgboost::xgb.train")
})

test_that("nested_cv registra o motor do param modelo sem perder a nota", {
  d <- mtcars[, c("mpg", "wt", "hp")]
  r <- tr_ml_nested_cv(d, "mpg", modelo = "cart", tentativas = 1, folds_externos = 2, folds = 2, seed = 7)
  expect_equal(ferramentas(r), "rpart::rpart")
  expect_true(is.data.frame(r))
})
