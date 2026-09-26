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

# Oráculo de outro pacote: `car::Manova` (Fox & Weisberg), que monta as
# matrizes H e E por conta própria (SQ tipo II; com bloco antes e sem
# interação, tipo II do tratamento = sequencial). Estatística, F e p a 1e-8.
test_that("Pillai e Wilks batem com car::Manova, com e sem bloco", {
  skip_if_not_installed("car")
  ir <- tr_multi_example("iris")
  set.seed(1); ir$bloco <- factor(rep(1:5, 30))
  Y <- as.matrix(ir[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])
  for (com in c(FALSE, TRUE)) {
    fit <- if (com) stats::lm(Y ~ bloco + Species, data = ir) else stats::lm(Y ~ Species, data = ir)
    for (e in c("Pillai", "Wilks")) {
      ref <- summary(car::Manova(fit), test = e)$multivariate.tests$Species
      eig <- Re(eigen(qr.coef(qr(ref$SSPE), ref$SSPH), only.values = TRUE)$values)
      st <- if (e == "Pillai") car:::Pillai(eig, ref$df, ref$df.residual) else car:::Wilks(eig, ref$df, ref$df.residual)
      t <- tr_multi_manova(ir, respostas = "Sepal.Length, Sepal.Width, Petal.Length",
                           tratamento = "Species", bloco = if (com) "bloco" else "", estatistica = e)
      expect_equal(t$estatistica, st[[2L]], tolerance = 1e-8, info = paste(e, com))
      expect_equal(t$extra[[tolower(e)]], st[[1L]], tolerance = 1e-8, info = paste(e, com))
    }
  }
})
