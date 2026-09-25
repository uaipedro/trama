# IC da AUC (DeLong, DeLong & Clarke-Pearson 1988) e corte de Youden (1950),
# conferidos contra pROC (Robin et al. 2011).

test_that("DeLong e Youden iguais ao pROC", {
  skip_if_not_installed("pROC")
  set.seed(3)
  y <- rep(c("nao", "sim"), c(60, 40))
  p <- round(plogis(c(rnorm(60, -.5), rnorm(40, .7))), 2)    # com empates
  d <- data.frame(y = y, .prob_sim = p)
  for (nivel in c(.9, .95, .99)) {
    z <- tr_ml_roc(d, "y", ".prob_sim", confianca = nivel)$data
    r <- pROC::roc(y, p, levels = c("nao", "sim"), direction = "<", quiet = TRUE)
    ci <- as.numeric(pROC::ci.auc(r, conf.level = nivel, method = "delong"))
    expect_equal(z$auc[[1]], as.numeric(r$auc), tolerance = 1e-10)
    expect_equal(c(z$auc_inf[[1]], z$auc_sup[[1]]), ci[c(1, 3)], tolerance = 1e-8)
    expect_equal(z$auc_ep[[1]]^2, as.numeric(pROC::var(r, method = "delong")), tolerance = 1e-8)
  }
  b <- pROC::coords(r, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"),
                    transpose = FALSE)
  expect_equal(nrow(b), 1L)
  i <- which(z$youden)
  expect_equal(z$youden_j[[1]], b$sensitivity + b$specificity - 1, tolerance = 1e-12)
  expect_equal(z$tpr[[i]], b$sensitivity, tolerance = 1e-12)
  expect_equal(1 - z$fpr[[i]], b$specificity, tolerance = 1e-12)
  # pROC usa o ponto médio entre valores vizinhos; o bloco dá o menor P incluído.
  expect_equal(z$youden_limiar[[1]], min(p[p > b$threshold]))
})

test_that("bordas: separação perfeita e classe com uma linha", {
  z <- tr_ml_roc(data.frame(y = c("a", "a", "b", "b"), .prob_b = c(.1, .2, .8, .9)), "y", ".prob_b")$data
  expect_equal(z$auc[[1]], 1)
  expect_equal(z$auc_ep[[1]], 0)                         # variância de DeLong nula
  expect_equal(c(z$auc_inf[[1]], z$auc_sup[[1]]), c(1, 1))
  expect_equal(z$youden_j[[1]], 1); expect_equal(z$youden_limiar[[1]], .8)
  u <- tr_ml_roc(data.frame(y = c("a", "b", "b"), .prob_b = c(.1, .8, .9)), "y", ".prob_b")$data
  expect_true(is.na(u$auc_inf[[1]]))
  expect_error(tr_ml_roc(data.frame(y = c("a", "b"), .prob_b = c(.1, .8)), "y", ".prob_b", confianca = 1),
               class = "tr_ml_error_bad_param")
})
