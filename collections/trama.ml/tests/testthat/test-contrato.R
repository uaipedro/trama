# O contrato de modelo da trama.models, pelos métodos da classe `tr_ml_fit`.

test_that("info descreve o modelo pelos nomes originais", {
  m <- tr_ml_cart(tr_ml_example("iris_binaria"), "Species")
  i <- trama.models::tr_models_info(m)
  expect_equal(i$tarefa, "classificacao")
  expect_equal(i$resposta, "Species")
  expect_equal(i$niveis, c("versicolor", "virginica"))
  expect_equal(i$n, 100L)
  expect_match(i$rotulo, "CART")
  expect_equal(names(m$dados), c("Species", m$preditores))
  # RDS de antes da Fase 4 guardava `alvo`.
  velho <- m; velho$resposta <- NULL; velho$alvo <- "Species"
  expect_equal(trama.models::tr_models_info(velho)$resposta, "Species")
  expect_equal(trama.models::tr_models_info(tr_ml_linear(mtcars, "am", "wt", tarefa = "classificacao"))$familia,
               "binomial")
})

test_that("predict_raw devolve fator nos níveis e probabilidades n × k", {
  m <- tr_ml_cart(iris, "Species", "Petal.Length, Petal.Width")
  p <- trama.models::tr_models_predict_raw(m, iris[c(1, 51, 101), ])
  expect_equal(levels(p$previsto), levels(iris$Species))
  expect_equal(dim(p$prob), c(3L, 3L))
  expect_equal(colnames(p$prob), levels(iris$Species))
  expect_equal(unname(rowSums(p$prob)), rep(1, 3))
  r <- trama.models::tr_models_predict_raw(tr_ml_linear(mtcars, "mpg", "wt"), mtcars[1:2, ])
  expect_type(r$previsto, "double")
  expect_null(r$prob)
})

test_that("predict_cv: resubstituição prevê o treino; cruzada é 5-fold reproduzível", {
  d <- tr_ml_example("iris_binaria")
  m <- tr_ml_cart(d, "Species", seed = 3)
  re <- trama.models::tr_models_predict_cv(m, "resubstituição")
  expect_equal(re$previsto, trama.models::tr_models_predict_raw(m, d)$previsto)
  cv <- trama.models::tr_models_predict_cv(m, "cruzada")
  expect_length(cv$previsto, nrow(d))
  expect_false(anyNA(cv$previsto))
  expect_equal(cv, trama.models::tr_models_predict_cv(m, "cruzada"))
  # Cada fold é um reajuste sem ele: o fold 1 sai igual ao modelo ajustado à mão.
  folds <- .tr_ml_with_seed(3, .tr_ml_make_folds(factor(d$Species), 5L))
  f1 <- folds[[1]]
  mao <- tr_ml_cart(d[-f1, ], "Species", seed = 3)
  expect_equal(as.character(cv$previsto[f1]),
               as.character(trama.models::tr_models_predict_raw(mao, d[f1, ])$previsto))
  # Só modelo: os avaliadores da models medem pela cruzada, com o real de $dados.
  ev <- trama.models::tr_models_evaluate(m)
  expect_equal(ev$valor[ev$metrica == "accuracy"], mean(as.character(cv$previsto) == as.character(d$Species)))
})

test_that("cruzada recusa classe com uma linha só", {
  d <- data.frame(x = c(1:9, 20), y = factor(c(rep("a", 9), "b")))
  m <- tr_ml_cart(d, "y", min_n = 1)
  expect_error(trama.models::tr_models_predict_cv(m, "cruzada"), class = "tr_models_error_not_applicable")
})

test_that("stats, resid, importance e coefs seguem o contrato", {
  m <- tr_ml_forest(mtcars, "mpg", "wt, hp", trees = 20L)
  s <- trama.models::tr_models_stats(m)
  expect_equal(nrow(s), 1L)
  expect_true(all(c("modelo", "n", "trees", "mtry", "min_n", "max_depth") %in% names(s)))
  r <- trama.models::tr_models_resid(m)
  expect_equal(r$residuo, r$mpg - r$ajustado)
  expect_error(trama.models::tr_models_resid(tr_ml_cart(iris, "Species")),
               class = "tr_models_error_not_applicable")
  expect_error(trama.models::tr_models_importance(tr_ml_svm(mtcars, "mpg", "wt")),
               class = "tr_models_error_not_applicable")
  expect_error(trama.models::tr_models_coefs(m), class = "tr_models_error_not_applicable")

  lin <- tr_ml_linear(mtcars, "mpg", "wt, hp")
  cf <- trama.models::tr_models_coefs(lin)
  ref <- stats::coef(summary(stats::lm(mpg ~ wt + hp, mtcars)))
  expect_equal(cf$tabela$termo, c("(Intercept)", "wt", "hp"))
  expect_equal(unname(cf$tabela$estimativa), unname(ref[, 1]))
  expect_equal(cf$coluna_estat, "t")
  imp <- trama.models::tr_models_importance(lin)
  expect_equal(imp$medida, c("|t|", "|t|"))
  expect_equal(sort(imp$importancia), sort(abs(unname(ref[-1, 3]))))
})

test_that("card e tabela: regras no CART, resumo nos demais", {
  m <- tr_ml_cart(mtcars, "mpg", "wt")
  expect_equal(trama.models::tr_models_as_table(m), tr_ml_rules(m))
  expect_equal(trama.models::tr_models_as_table(tr_ml_linear(mtcars, "mpg", "wt"))$resposta, "mpg")
})
