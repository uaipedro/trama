# IC da AUC (DeLong, DeLong & Clarke-Pearson 1988), corte de Youden (1950) e
# AUC multiclasse de Hand & Till (2001) na `models/roc`, conferidos contra
# pROC (Robin et al. 2011). Portes dos testes da `ml/roc` (test-roc-ic.R) e da
# `multi/roc` (test-roc-delong.R, test-roc-hand-till.R) da main; os que
# precisam de um classificador da `multi` ficam na suíte da multi.

roc_tab <- function(d, prob, ...) tr_models_roc(dados = d, resposta = "y", probabilidade = prob, ...)$data

test_that("DeLong e Youden iguais ao pROC", {
  skip_if_not_installed("pROC")
  set.seed(3)
  y <- rep(c("nao", "sim"), c(60, 40))
  p <- round(plogis(c(rnorm(60, -.5), rnorm(40, .7))), 2)    # com empates
  d <- data.frame(y = y, .prob_sim = p)
  for (nivel in c(.9, .95, .99)) {
    z <- roc_tab(d, ".prob_sim", confianca = nivel)
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

test_that("AUC e IC de DeLong batem com o pROC (aSAH, com empates)", {
  skip_if_not_installed("pROC")
  e <- new.env(); utils::data("aSAH", package = "pROC", envir = e)
  a <- e$aSAH
  oraculo <- function(score, positivo, conf = 0.95) {
    r <- pROC::roc(controls = score[!positivo], cases = score[positivo], direction = "<", quiet = TRUE)
    as.numeric(pROC::ci.auc(r, conf.level = conf, method = "delong"))
  }
  for (v in c("s100b", "ndka", "wfns")) {
    s <- as.numeric(a[[v]]); pos <- a$outcome == "Poor"
    r <- .tr_models_auc_delong(s, pos, 0.95)
    expect_equal(c(r$ic_inf, r$auc, r$ic_sup), oraculo(s, pos), tolerance = 1e-8)
    expect_equal(r$auc, .tr_models_roc_curva(s, pos)$auc, tolerance = 1e-12)
  }
  s <- a$s100b; pos <- a$outcome == "Poor"
  expect_equal(unlist(.tr_models_auc_delong(s, pos, 0.9)[c("ic_inf", "auc", "ic_sup")], use.names = FALSE),
               oraculo(s, pos, 0.9), tolerance = 1e-8)
})

test_that("bordas: separação perfeita e classe com uma linha", {
  z <- roc_tab(data.frame(y = c("a", "a", "b", "b"), .prob_b = c(.1, .2, .8, .9)), ".prob_b")
  expect_equal(z$auc[[1]], 1)
  expect_equal(z$auc_ep[[1]], 0)                         # variância de DeLong nula
  # IC degenerado: largura zero não é certeza, é o estimador sem informação
  expect_true(is.na(z$auc_inf[[1]])); expect_true(is.na(z$auc_sup[[1]]))
  expect_match(z$auc_nota[[1]], "degenerad")
  z0 <- roc_tab(data.frame(y = c("a", "a", "b", "b"), .prob_b = c(.9, .8, .2, .1)), ".prob_b")
  expect_equal(z0$auc[[1]], 0); expect_true(is.na(z0$auc_inf[[1]])); expect_match(z0$auc_nota[[1]], "degenerad")
  expect_true(is.na(roc_tab(data.frame(y = rep(c("a", "b"), 3), .prob_b = c(.1, .8, .4, .7, .6, .3)),
                            ".prob_b")$auc_nota[[1]]))
  expect_equal(z$youden_j[[1]], 1); expect_equal(z$youden_limiar[[1]], .8)
  u <- roc_tab(data.frame(y = c("a", "b", "b"), .prob_b = c(.1, .8, .9)), ".prob_b")
  expect_true(is.na(u$auc_inf[[1]])); expect_true(is.na(u$auc_ep[[1]]))
  expect_match(u$auc_nota[[1]], "menos de duas")
  expect_error(tr_models_roc(dados = data.frame(y = c("a", "b"), .prob_b = c(.1, .8)), resposta = "y",
                             probabilidade = ".prob_b", confianca = 1), class = "tr_models_error_bad_option")
  # O gráfico não é recusado: o subtítulo diz indisponível e a legenda explica.
  p <- tr_models_roc(dados = data.frame(y = c("a", "a", "b", "b"), .prob_b = c(.1, .2, .8, .9)),
                     resposta = "y", probabilidade = ".prob_b")
  expect_match(p$labels$subtitle, "AUC 1,000 \\(IC indisponível\\)")
  expect_match(p$labels$caption, "degenerado")
})

test_that("M de Hand & Till: conta à mão com empates e 4 classes, pROC, e 2 classes reduz à AUC", {
  skip_if_not_installed("pROC")
  set.seed(7)
  g <- factor(rep(c("a", "b", "c", "d"), times = c(6, 5, 7, 4)))
  prob <- matrix(round(stats::runif(22 * 4), 1), ncol = 4, dimnames = list(NULL, levels(g)))
  prob <- prob / rowSums(prob)
  real <- as.character(g)
  ht <- .tr_models_auc_hand_till(prob, real, levels(g))
  expect_equal(ht, as.numeric(pROC::multiclass.roc(g, as.data.frame(prob), quiet = TRUE)$auc), tolerance = 1e-10)
  A <- function(i, j) {
    s <- prob[g %in% c(i, j), i]; pos <- g[g %in% c(i, j)] == i
    .tr_models_roc_curva(s, pos)$auc
  }
  pares <- utils::combn(levels(g), 2)
  mao <- mean(apply(pares, 2, function(p) (A(p[1], p[2]) + A(p[2], p[1])) / 2))
  expect_equal(ht, mao, tolerance = 1e-12)
  g2 <- c("x", "x", "x", "y", "y", "y")
  p2 <- cbind(x = c(.9, .6, .5, .4, .3, .5), y = 1 - c(.9, .6, .5, .4, .3, .5))
  expect_equal(.tr_models_auc_hand_till(p2, g2, c("x", "y")), .tr_models_roc_curva(p2[, "y"], g2 == "y")$auc)
  # No gráfico do modo tabela com as colunas de todas as classes: M no subtítulo.
  d <- data.frame(y = real, prob_a = prob[, "a"], prob_b = prob[, "b"], prob_c = prob[, "c"], prob_d = prob[, "d"])
  q <- tr_models_roc(dados = d, resposta = "y")
  expect_match(q$labels$subtitle, "AUC multiclasse \\(Hand & Till\\) [0-9],[0-9]{3}")
  expect_true(any(grepl("IC 95% DeLong", levels(q$layers[[2]]$data$grupo))))
})
