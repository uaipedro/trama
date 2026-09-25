# MANOVA conferida contra summary.manova, nas quatro estatísticas.

test_that("as quatro estatísticas batem com summary.manova (DIC)", {
  ir <- tr_multi_example("iris")
  Y <- as.matrix(ir[, 1:4])
  fit <- stats::manova(Y ~ ir$Species)
  for (e in c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")) {
    t <- tr_multi_manova(ir, respostas = "Sepal.Length, Sepal.Width, Petal.Length, Petal.Width",
                         tratamento = "Species", estatistica = e)
    ref <- summary(fit, test = e)$stats[1, ]
    expect_equal(t$estatistica, ref[[3]], info = e)
    expect_equal(t$p_valor, ref[[6]], info = e)
  }
})

test_that("com bloco o tratamento é testado descontado o bloco", {
  set.seed(3)
  d <- data.frame(trat = rep(c("a", "b", "c"), each = 4), bloco = rep(1:4, 3))
  d$y1 <- rnorm(12) + d$bloco + (d$trat == "b")
  d$y2 <- rnorm(12) + d$bloco
  t <- tr_multi_manova(d, respostas = "y1, y2", tratamento = "trat", bloco = "bloco")
  fit <- stats::manova(cbind(y1, y2) ~ factor(bloco) + trat, data = d)
  ref <- summary(fit)$stats["trat", ]
  expect_equal(t$estatistica, ref[[3]])
  expect_equal(t$p_valor, ref[[6]])
})

test_that("recusas: respostas em branco, uma resposta só, tratamento de um nível", {
  ir <- tr_multi_example("iris")
  expect_error(tr_multi_manova(ir, tratamento = "Species"), class = "tr_multi_error_blank_param")
  expect_error(tr_multi_manova(ir, respostas = "Sepal.Length", tratamento = "Species"),
               class = "tr_multi_error_too_few_variables")
  ir1 <- ir[ir$Species == "setosa", ]
  expect_error(tr_multi_manova(ir1, respostas = "Sepal.Length, Sepal.Width", tratamento = "Species"),
               class = "tr_multi_error_one_group")
})
