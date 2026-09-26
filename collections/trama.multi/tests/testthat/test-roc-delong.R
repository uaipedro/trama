# IC da AUC por DeLong, DeLong & Clarke-Pearson (1988): variância pelas
# componentes estruturais (placements). Oráculo: `pROC::ci.auc(method =
# "delong")`, direção fixada (positivo com escore maior), a 1e-8.

oraculo_proc <- function(score, positivo, conf = 0.95) {
  r <- pROC::roc(controls = score[!positivo], cases = score[positivo], direction = "<", quiet = TRUE)
  as.numeric(pROC::ci.auc(r, conf.level = conf, method = "delong"))
}

test_that("AUC e IC de DeLong batem com o pROC (aSAH, com empates)", {
  skip_if_not_installed("pROC")
  e <- new.env(); utils::data("aSAH", package = "pROC", envir = e)
  a <- e$aSAH
  for (v in c("s100b", "ndka", "wfns")) {
    s <- as.numeric(a[[v]]); pos <- a$outcome == "Poor"
    r <- .tr_multi_auc_delong(s, pos, 0.95)
    o <- oraculo_proc(s, pos)
    expect_equal(c(r$ic_inf, r$auc, r$ic_sup), o, tolerance = 1e-8)
  }
  s <- a$s100b; pos <- a$outcome == "Poor"
  expect_equal(unlist(.tr_multi_auc_delong(s, pos, 0.9)[c("ic_inf", "auc", "ic_sup")], use.names = FALSE),
               oraculo_proc(s, pos, 0.9), tolerance = 1e-8)
})

test_that("nas probabilidades de deixa-um-fora da logística do pima, idem", {
  skip_if_not_installed("pROC")
  m <- tr_multi_logistic(tr_multi_example("pima"), resposta = "diabetes")
  pr <- .tr_multi_prever(m, "cruzada", "multi/roc")
  s <- pr$prob[, "sim"]; pos <- pr$g == "sim"
  r <- .tr_multi_auc_delong(s, pos, 0.95)
  expect_equal(c(r$ic_inf, r$auc, r$ic_sup), oraculo_proc(s, pos), tolerance = 1e-8)
  expect_equal(r$auc, .tr_multi_roc_curva(s, pos)$auc, tolerance = 1e-12)
})

test_that("bordas: AUC 0/1 e classe com menos de 2 dão IC NA com nota (como a ml/roc)", {
  # Separação perfeita: variância de DeLong zero, intervalo sem informação.
  r <- .tr_multi_auc_delong(c(1, 2, 3, 4), c(FALSE, FALSE, TRUE, TRUE), 0.95)
  expect_equal(r$auc, 1)
  expect_true(is.na(r$ic_inf) && is.na(r$ic_sup))
  expect_match(r$nota, "degenerado")
  r <- .tr_multi_auc_delong(c(4, 3, 2, 1), c(FALSE, FALSE, TRUE, TRUE), 0.95)
  expect_equal(r$auc, 0)
  expect_true(is.na(r$ic_inf))
  r <- .tr_multi_auc_delong(c(1, 3, 2, 4, 5), c(FALSE, TRUE, FALSE, FALSE, TRUE), 0.95)
  expect_true(r$ic_inf >= 0 && r$ic_sup <= 1)
  expect_true(is.na(r$nota))
  r <- .tr_multi_auc_delong(c(1, 2, 3), c(FALSE, TRUE, FALSE), 0.95)
  expect_equal(r$auc, 0.5)
  expect_true(is.na(r$ic_inf) && is.na(r$ic_sup))
  expect_match(r$nota, "menos de duas")
})

test_that("gráfico: IC indisponível de um grupo não derruba a ROC nem os outros", {
  # Um grupo com menos de 2 casos não chega aqui (o ajuste recusa); a borda
  # que chega é a AUC 1 de um grupo separado, como a setosa na LDA da iris.
  l <- tr_multi_discriminant(iris, resposta = "Species")
  q <- tr_multi_roc(l)
  rot <- levels(q$layers[[2]]$data$grupo)
  expect_true(any(grepl("^setosa \\(AUC 1,000 \\(IC indisponível\\)", rot)))
  expect_true(any(grepl("^versicolor .*IC 95% DeLong", rot)))
  expect_match(q$labels$caption, "degenerado")
  # Dois grupos com AUC 1: o subtítulo diz indisponível e a legenda explica.
  d2 <- droplevels(as.data.frame(tr_multi_example("iris"))[1:100, ])
  m <- tr_multi_discriminant(d2, resposta = "Species")
  p <- tr_multi_roc(m, validacao = "resubstituição")
  expect_match(p$labels$subtitle, "AUC 1,000 \\(IC indisponível\\)")
  expect_match(p$labels$caption, "degenerado")
})

test_that("o gráfico leva o IC no subtítulo e na legenda, e aceita confianca", {
  m <- tr_multi_logistic(tr_multi_example("pima"), resposta = "diabetes")
  p <- tr_multi_roc(m, confianca = 0.9)
  expect_match(p$labels$subtitle, "IC 90% DeLong")
  l <- tr_multi_discriminant(iris, resposta = "Species")
  q <- tr_multi_roc(l)
  expect_true(any(grepl("IC 95%", levels(q$layers[[2]]$data$grupo))))
  expect_error(tr_multi_roc(m, confianca = 2), class = "tr_models_error_bad_option")
})
