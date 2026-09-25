test_that("split estratifica, preserva linhas e restaura RNG", {
  d <- tibble::tibble(y = factor(rep(c("a", "b"), each = 10)), x = seq_len(20))
  set.seed(99); referencia <- c(runif(1), runif(1))
  set.seed(99); s <- tr_ml_split(d, "y", 0.75, seed = 7)
  set.seed(99); antes <- runif(1)
  depois <- runif(1)
  expect_equal(c(antes, depois), referencia)
  expect_equal(nrow(s$treino) + nrow(s$teste), nrow(d))
  expect_equal(sort(c(s$treino$x, s$teste$x)), d$x)
  expect_true(all(table(s$treino$y) > 0))
  expect_true(all(table(s$teste$y) > 0))
})

test_that("split recusa classe singleton e valida alvo", {
  d <- tibble::tibble(y = factor(c("a", "a", "b")), x = 1:3)
  expect_error(tr_ml_split(d, "y"), "singleton")
  expect_error(tr_ml_split(d, "ausente"), "coluna existente")
  expect_error(tr_ml_split(d, "y", proporcao = 1), "entre 0 e 1")
  expect_error(tr_ml_split(d, "y", seed = 1.5), "inteiro")
  expect_error(tr_ml_split(d, "y", estratificar = 1), "TRUE ou FALSE")
})

test_that("split numérico não duplica nem descarta", {
  d <- tibble::tibble(y = seq_len(9), x = letters[seq_len(9)])
  s <- tr_ml_split(d, "y", proporcao = 0.6, estratificar = TRUE)
  expect_equal(nrow(s$treino), 5)
  expect_equal(sort(c(s$treino$y, s$teste$y)), d$y)
})

test_that("métricas de regressão e R2 indefinido", {
  d <- tibble::tibble(y = c(1, 2, 4), .pred = c(2, 2, 3))
  z <- tr_ml_evaluate(d, "y")
  expect_equal(z$metrica, c("mae", "rmse", "r2"))
  expect_equal(z$valor[1], 2/3)
  expect_equal(z$valor[3], 1 - 2/(14/3))
  expect_true(is.na(tr_ml_evaluate(tibble::tibble(y = c(2, 2), .pred = c(1, 3)), "y")$valor[3]))
  expect_error(tr_ml_evaluate(tibble::tibble(y = c(1, Inf), .pred = c(1, 2)), "y"), "finitos")
  expect_error(tr_ml_evaluate(tibble::tibble(y = numeric(), .pred = numeric()), "y"), "pelo menos uma")
})

test_that("classificação calcula macro e aceita classe ausente na previsão", {
  d <- tibble::tibble(y = factor(c("a", "a", "b", "b")), .pred = factor(c("a", "a", "a", "a"), levels = c("a", "b")))
  z <- tr_ml_evaluate(d, "y")
  expect_equal(z$valor[1:3], c(0.5, 0.5, 1/3))
  m <- tr_ml_confusion(d, "y")
  expect_equal(sum(m$n), 4)
  expect_equal(m$n[m$observado == "b" & m$previsto == "a"], 2)
})

test_that("classes previstas fora do alvo não alteram a média sobre classes observadas", {
  d <- tibble::tibble(y = c("a", "a", "b", "b"), .pred = c("a", "a", "c", "c"))
  z <- tr_ml_evaluate(d, "y")
  expect_equal(z$valor[1:3], c(0.5, 0.5, 0.5))
  expect_equal(sum(tr_ml_confusion(d, "y")$n), 4)
})

test_that("classificação rejeita valores numéricos não finitos", {
  expect_error(
    tr_ml_evaluate(tibble::tibble(y = c(1, 2), .pred = c(1, Inf)), "y",
                   tarefa = "classificacao"),
    "finitos"
  )
})

test_that("classificação aceita códigos numéricos na avaliação e confusão", {
  d <- tibble::tibble(y = c(0, 0, 1, 1), .pred = c(0, 1, 1, 1))
  z <- tr_ml_evaluate(d, "y", tarefa = "classificacao")
  expect_equal(z$valor[1:3], c(0.75, 0.75, 11/15))
  m <- tr_ml_confusion(d, "y")
  expect_equal(sum(m$n), 4)
  expect_equal(m$n[m$observado == "0" & m$previsto == "1"], 1)
})

test_that("exemplos têm tamanhos e alvos esperados", {
  expect_equal(nrow(tr_ml_example("iris")), 150)
  expect_equal(nrow(tr_ml_example("iris_binaria")), 100)
  expect_equal(nlevels(tr_ml_example("iris_binaria")$Species), 2)
  expect_equal(nrow(tr_ml_example("mtcars")), 32)
  expect_error(tr_ml_example("desconhecido"), "deve ser")
})

test_that("métricas não descartam faltantes silenciosamente", {
  expect_error(tr_ml_evaluate(tibble::tibble(y = c(1, NA), .pred = c(1, 2)), "y"), "ausentes")
  expect_error(tr_ml_confusion(tibble::tibble(y = c("a", NA), .pred = c("a", "a")), "y"), "ausentes")
})
test_that("divisao independe do gerador do chamador e o restaura", {
  kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(kind)))
  RNGkind("Mersenne-Twister")
  a <- tr_ml_split(iris, "Species")
  RNGkind("L'Ecuyer-CMRG")
  set.seed(32)
  antes <- .Random.seed
  b <- tr_ml_split(iris, "Species")
  expect_identical(a, b)
  expect_identical(.Random.seed, antes)
  expect_identical(RNGkind()[[1]], "L'Ecuyer-CMRG")
})
