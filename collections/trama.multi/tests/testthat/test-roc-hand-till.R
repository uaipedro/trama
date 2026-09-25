# AUC multiclasse M de Hand & Till (2001, doi:10.1023/A:1010920819831): média,
# sobre os pares de grupos, de Â(i, j) = [A(i|j) + A(j|i)] / 2, cada A só com
# os casos dos dois grupos e o escore do grupo tomado como positivo. Oráculo:
# `pROC::multiclass.roc` com a matriz de probabilidades, a 1e-10.

oraculo_ht <- function(prob, g) {
  as.numeric(pROC::multiclass.roc(g, as.data.frame(prob), quiet = TRUE)$auc)
}

test_that("M de Hand & Till bate com o pROC (LDA e logística, 3 grupos)", {
  skip_if_not_installed("pROC")
  l <- tr_multi_discriminant(tr_multi_example("iris"), grupo = "Species")
  pr <- .tr_multi_prever(l, "cruzada", "multi/roc")
  expect_equal(.tr_multi_auc_hand_till(pr$prob, pr$g), oraculo_ht(pr$prob, pr$g), tolerance = 1e-10)
  v <- tr_multi_logistic(tr_multi_example("vinhos"), grupo = "cultivar",
                         cols = "alcool, acidez_malica, magnesio, fenois_totais")
  pr <- .tr_multi_prever(v, "cruzada", "multi/roc")
  expect_equal(.tr_multi_auc_hand_till(pr$prob, pr$g), oraculo_ht(pr$prob, pr$g), tolerance = 1e-10)
})

test_that("conta à mão com empates e 4 grupos; 2 grupos reduz à AUC", {
  skip_if_not_installed("pROC")
  set.seed(7)
  g <- factor(rep(c("a", "b", "c", "d"), times = c(6, 5, 7, 4)))
  prob <- matrix(round(stats::runif(22 * 4), 1), ncol = 4, dimnames = list(NULL, levels(g)))
  prob <- prob / rowSums(prob)
  expect_equal(.tr_multi_auc_hand_till(prob, g), oraculo_ht(prob, g), tolerance = 1e-10)
  # À mão: par (a, b) só com o escore de a.
  A <- function(i, j) {
    s <- prob[g %in% c(i, j), i]; pos <- g[g %in% c(i, j)] == i
    .tr_multi_roc_curva(s, pos)$auc
  }
  pares <- utils::combn(levels(g), 2)
  mao <- mean(apply(pares, 2, function(p) (A(p[1], p[2]) + A(p[2], p[1])) / 2))
  expect_equal(.tr_multi_auc_hand_till(prob, g), mao, tolerance = 1e-12)
  g2 <- factor(rep(c("x", "y"), c(3, 3)))
  p2 <- cbind(x = c(.9, .6, .5, .4, .3, .5), y = 1 - c(.9, .6, .5, .4, .3, .5))
  expect_equal(.tr_multi_auc_hand_till(p2, g2), .tr_multi_roc_curva(p2[, "y"], g2 == "y")$auc)
})

test_that("o gráfico de 3+ grupos leva o M no subtítulo", {
  l <- tr_multi_discriminant(tr_multi_example("iris"), grupo = "Species")
  p <- tr_multi_roc(l)
  expect_match(p$labels$subtitle, "AUC multiclasse \\(Hand & Till\\) [0-9],[0-9]{3}")
})
