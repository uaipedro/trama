test_that("tuning escolhe por validação cruzada e reajusta em todo treino", {
  d <- mtcars[, c("mpg", "wt", "hp")]
  a <- tr_ml_tune(d, "mpg", modelo = "cart", tentativas = 3, folds = 3, seed = 19)
  b <- tr_ml_tune(d, "mpg", modelo = "cart", tentativas = 3, folds = 3, seed = 19)

  expect_s3_class(a, "tr_ml_tuning")
  expect_s3_class(a$modelo, "tr_ml_fit")
  expect_equal(a$modelo$n, nrow(d))
  expect_equal(a$historico, b$historico)
  expect_equal(nrow(a$historico), 3L)
  expect_true(all(a$historico$status == "ok"))
  expect_equal(a$melhor_tentativa, which.min(a$historico$media))
})

test_that("tuning maximiza classificação sem tocar no RNG do chamador", {
  d <- subset(iris, Species != "virginica")
  set.seed(812)
  antes <- .Random.seed
  z <- tr_ml_tune(d, "Species", modelo = "cart", tentativas = 2, folds = 3, seed = 7)
  expect_identical(.Random.seed, antes)
  expect_equal(z$metrica, "macro_f1")
  expect_equal(z$melhor_tentativa, which.max(z$historico$media))
})

test_that("tuning valida orçamento, folds e modelos ajustáveis", {
  expect_error(tr_ml_tune(mtcars, "mpg", modelo = "linear"),
               class = "tr_ml_error_not_tunable")
  expect_error(tr_ml_tune(mtcars, "mpg", tentativas = 0),
               class = "tr_ml_error_bad_param")
  expect_error(tr_ml_tune(mtcars[1:3, ], "mpg", folds = 4),
               class = "tr_ml_error_bad_folds")
})
