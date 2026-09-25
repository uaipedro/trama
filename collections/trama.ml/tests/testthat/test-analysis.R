test_that("visualizador desenha CART e recusa modelos sem árvore legível", {
  m <- tr_ml_cart(mtcars, "mpg", "wt, hp", max_depth = 2, min_n = 3)
  p <- tr_ml_tree_plot(m)
  expect_s3_class(p, "ggplot")
  expect_gt(nrow(p$data), 1L)
  expect_equal(attr(p, "tr_view_dim"), c(8, 4.5))
  expect_true(any(grepl("sim|não", ggplot2::layer_data(p, 2)$label)))
  raiz <- p$data[p$data$id == 1L, ]
  operador <- if (m$ajuste$splits[1, "ncat"] < 0) " < " else " >= "
  expect_match(raiz$decisao, operador, fixed = TRUE)

  compacto <- tr_ml_tree_plot(m, mostrar_n = FALSE, mostrar_impureza = TRUE, casas = 2)
  expect_false(any(grepl("n =", compacto$data$label, fixed = TRUE)))
  expect_true(any(grepl("impureza", compacto$data$label, fixed = TRUE)))

  linear <- tr_ml_linear(mtcars, "mpg", "wt")
  expect_error(tr_ml_tree_plot(linear), class = "tr_ml_error_not_applicable")

  if (requireNamespace("figsr", quietly = TRUE)) {
    d <- subset(iris, Species != "virginica")
    figs <- tr_ml_figs(d, "Species", max_splits = 3)
    pf <- tr_ml_tree_plot(figs, mostrar_impureza = TRUE)
    expect_s3_class(pf, "ggplot")
    expect_true(any(grepl("ganho", pf$data$label, fixed = TRUE)))
  }
})

test_that("diagnóstico de resíduos mostra erro contra previsão", {
  d <- tibble::tibble(y = c(1, 2, 4), .pred = c(1.2, 1.8, 3.5))
  p <- tr_ml_residuals(d, "y")
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$.residuo, d$y - d$.pred)
  expect_error(tr_ml_residuals(transform(d, y = factor(y)), "y"),
               class = "tr_ml_error_not_applicable")
})

test_that("curva ROC usa probabilidades e inclui os extremos", {
  d <- tibble::tibble(y = factor(c("nao", "sim", "nao", "sim")),
                      .prob_sim = c(.1, .8, .4, .7))
  p <- tr_ml_roc(d, "y", ".prob_sim", positiva = "sim")
  expect_s3_class(p, "ggplot")
  expect_equal(p$data[1, c("fpr", "tpr")], tibble::tibble(fpr = 0, tpr = 0))
  expect_equal(tail(p$data[c("fpr", "tpr")], 1), tibble::tibble(fpr = 1, tpr = 1))
  expect_equal(unique(p$data$auc), 1)

  empate <- tibble::tibble(y = factor(c("nao", "sim")), .prob_sim = c(.5, .5))
  expect_equal(unique(tr_ml_roc(empate, "y", ".prob_sim", "sim")$data$auc), .5)
})

test_that("histórico de tuning vira gráfico e valida o contrato", {
  h <- tibble::tibble(tentativa = 1:3, media = c(.7, .8, .75),
                      melhor = c(.7, .8, .8), max_depth = c(2, 4, 6), status = "ok")
  expect_s3_class(tr_ml_tuning_plot(h), "ggplot")
  por_parametro <- tr_ml_tuning_plot(h, "max_depth")
  expect_equal(por_parametro$labels$x, "max_depth")
  expect_error(tr_ml_tuning_plot(h, "ausente"), class = "tr_ml_error_bad_tuning")
  expect_error(tr_ml_tuning_plot(h[, -2]), class = "tr_ml_error_bad_tuning")
})

test_that("ROC deduz a classe positiva do nome da coluna de probabilidade", {
  # Regressão: com `positiva` vazia, a segunda classe na ordem das linhas era
  # "nao" e a curva saía espelhada (AUC 0 em vez de 1).
  d <- tibble::tibble(y = c("sim", "nao", "sim", "nao"), .prob_sim = c(.8, .1, .7, .4))
  expect_equal(unique(tr_ml_roc(d, "y", ".prob_sim")$data$auc), 1)
  d2 <- tibble::tibble(y = d$y, .prob_nao = 1 - d$.prob_sim)
  expect_equal(unique(tr_ml_roc(d2, "y", ".prob_nao")$data$auc), 1)
  # Sem `.prob_<classe>` reconhecível, a classe não é adivinhada: exige `positiva`.
  d3 <- tibble::tibble(y = d$y, escore = d$.prob_sim)
  expect_error(tr_ml_roc(d3, "y", "escore"), class = "tr_ml_error_positive_required")
  d4 <- tibble::tibble(y = factor(d$y, levels = c("sim", "nao")), escore = d$.prob_sim)
  expect_error(tr_ml_roc(d4, "y", "escore"), class = "tr_ml_error_positive_required")
  d5 <- tibble::tibble(y = d$y, .prob_talvez = d$.prob_sim)
  expect_error(tr_ml_roc(d5, "y", ".prob_talvez"), class = "tr_ml_error_positive_required")
  expect_equal(unique(tr_ml_roc(d4, "y", "escore", positiva = "sim")$data$auc), 1)
})

test_that("AUC coincide com a estatística de Mann-Whitney (Hanley & McNeil 1982)", {
  # AUC = U / (n1 n0), com empates valendo 1/2 (Hanley & McNeil 1982, eq. 1).
  y <- c("a", "b", "b", "a", "b", "a", "b", "b", "a", "a")
  p <- c(.2, .9, .5, .5, .7, .1, .3, .8, .6, .4)
  auc <- unique(tr_ml_roc(data.frame(y = y, .prob_b = p), "y", ".prob_b")$data$auc)
  u <- suppressWarnings(stats::wilcox.test(p[y == "b"], p[y == "a"], exact = FALSE))$statistic
  expect_equal(auc, unname(u) / (5 * 5), tolerance = 1e-12)
  expect_equal(auc, 0.82, tolerance = 1e-12)
})
