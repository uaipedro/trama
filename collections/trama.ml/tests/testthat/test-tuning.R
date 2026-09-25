test_that("tuning escolhe por validação cruzada e reajusta em todo treino", {
  d <- mtcars[, c("mpg", "wt", "hp")]
  a <- tr_ml_tune(d, "mpg", modelo = "cart", tentativas = 3, folds = 3, seed = 19)
  b <- tr_ml_tune(d, "mpg", modelo = "cart", tentativas = 3, folds = 3, seed = 19)

  expect_s3_class(a, "tr_ml_tuning")
  expect_s3_class(a$modelo, "tr_ml_fit")
  expect_equal(a$modelo$n, nrow(d))
  expect_equal(a$historico[names(a$historico) != "segundos"],
               b$historico[names(b$historico) != "segundos"])
  expect_equal(nrow(a$historico), 3L)
  expect_true(all(a$historico$status == "ok"))
  expect_true(all(c("fold_1", "fold_2", "fold_3", "segundos", "avisos") %in%
                  names(a$historico)))
  expect_true(all(a$historico$segundos >= 0))
  expect_equal(a$melhor_tentativa, which.min(a$historico$media))
})

test_that("tuning percorre todos os motores disponíveis", {
  d <- subset(iris, Species != "virginica")
  pacotes <- c(figs = "figsr", forest = "ranger", svm = "e1071", xgboost = "xgboost")
  for (modelo in names(pacotes)) {
    if (!requireNamespace(pacotes[[modelo]], quietly = TRUE)) next
    z <- tr_ml_tune(d, "Species", modelo = modelo, tentativas = 1, folds = 2, seed = 31)
    expect_s3_class(z$modelo, "tr_ml_fit")
    expect_equal(nrow(z$historico), 1L, info = modelo)
    expect_equal(z$historico$status, "ok", info = modelo)
  }
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

test_that("modelo tunado recusa a cruzada (viés de seleção), aceita dados e resubstituição", {
  skip_if_not_installed("trama.models")
  z <- tr_ml_tune(iris, "Species", modelo = "cart", tentativas = 3, folds = 3, seed = 7)
  expect_true(isTRUE(z$modelo$extras$tunado))
  expect_error(trama.models::tr_models_evaluate(z$modelo, validacao = "cruzada"),
               class = "tr_models_error_not_applicable")
  expect_s3_class(trama.models::tr_models_evaluate(z$modelo, dados = iris), "data.frame")
  expect_s3_class(trama.models::tr_models_evaluate(z$modelo, validacao = "resubstituição"), "data.frame")
  m <- tr_ml_fit(iris, "Species", modelo = "cart")
  expect_null(m$extras$tunado)
  expect_s3_class(trama.models::tr_models_evaluate(m, validacao = "cruzada"), "data.frame")
})
