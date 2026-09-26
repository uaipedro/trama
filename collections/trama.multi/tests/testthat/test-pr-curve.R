# Curva precisão-revocação: AP = Σ ΔR·P (sem interpolação) e área de Davis &
# Goadrich (2006) integrada em forma fechada (Keilwagen, Grosse & Grau 2014),
# a mesma conta da `ml/pr_curve`. Oráculos: `yardstick::average_precision`
# (AP) e `PRROC::pr.curve` (`auc.integral`), e o exemplo à mão da ml.

test_that("exemplo à mão (o mesmo da ml/pr_curve)", {
  z <- .tr_multi_pr_pontos(c(TRUE, FALSE, TRUE, FALSE, TRUE), c(.9, .8, .7, .6, .2))
  expect_equal(z$recall, c(1, 1, 2, 2, 3) / 3)
  expect_equal(z$precision, c(1, 1 / 2, 2 / 3, 1 / 2, 3 / 5))
  expect_equal(z$ap[[1]], (1 + 2 / 3 + 3 / 5) / 3, tolerance = 1e-12)
  expect_equal(z$area[[1]], (1 + 1 - log(3 / 2) + 1 - 2 * log(5 / 4)) / 3, tolerance = 1e-12)
  expect_equal(z$prevalencia[[1]], 0.6)
})

oraculos_pr <- function(s, pos) {
  ap <- yardstick::average_precision(
    data.frame(t = factor(ifelse(pos, "p", "n"), levels = c("p", "n")), s = s), t, s)$.estimate
  area <- PRROC::pr.curve(scores.class0 = s[pos], scores.class1 = s[!pos])$auc.integral
  c(ap = ap, area = area)
}

test_that("oráculos: pima por deixa-um-fora (binária) e vinhos (cada grupo contra os outros)", {
  skip_if_not_installed("yardstick")
  skip_if_not_installed("PRROC")
  m <- tr_multi_logistic(tr_multi_example("pima"), resposta = "diabetes")
  pr <- .tr_multi_prever(m, "cruzada", "multi/pr_curve")
  s <- pr$prob[, "sim"]; pos <- pr$g == "sim"
  z <- .tr_multi_pr_pontos(pos, s)
  o <- oraculos_pr(s, pos)
  expect_equal(z$ap[[1]], o[["ap"]], tolerance = 1e-10)
  expect_equal(z$area[[1]], o[["area"]], tolerance = 1e-8)
  v <- tr_multi_discriminant(tr_multi_example("vinhos"), resposta = "cultivar")
  pr <- .tr_multi_prever(v, "cruzada", "multi/pr_curve")
  for (l in levels(pr$g)) {
    s <- round(pr$prob[, l], 2); pos <- pr$g == l             # com empates
    z <- .tr_multi_pr_pontos(pos, s)
    o <- oraculos_pr(s, pos)
    expect_equal(z$ap[[1]], o[["ap"]], tolerance = 1e-10, info = l)
    expect_equal(z$area[[1]], o[["area"]], tolerance = 1e-8, info = l)
  }
})

test_that("o bloco: subtítulo com AP e acaso (binária), legenda por grupo (3+), bordas", {
  m <- tr_multi_logistic(tr_multi_example("pima"), resposta = "diabetes")
  p <- tr_multi_pr_curve(m)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "AP [0-9],[0-9]{3} · acaso [0-9],[0-9]{3}")
  expect_match(p$labels$subtitle, "positivo: sim")
  l <- tr_multi_discriminant(tr_multi_example("iris"), resposta = "Species")
  q <- tr_multi_pr_curve(l, validacao = "resubstituição")
  expect_true(any(grepl("AP", levels(q$data$grupo))))
  # Empate total: um ponto só, AP = prevalência.
  z <- .tr_multi_pr_pontos(c(TRUE, FALSE, TRUE, FALSE, TRUE), rep(.5, 5))
  expect_equal(nrow(z), 1L); expect_equal(z$ap[[1]], .6); expect_equal(z$area[[1]], .6)
  expect_error(tr_multi_pr_curve(m, validacao = "x"), class = "tr_models_error_bad_option")
  expect_error(tr_multi_pr_curve(iris), class = "tr_models_error_not_a_fit")
})

test_that("a multi/pr_curve da main abre como models/pr_curve, que lê o classificador", {
  reg <- multi_registry()
  doc <- list(nodes = list(n = list(type = "multi/pr_curve", params = list(validacao = "cruzada"))), edges = list())
  n <- trama::tr_doc_migrate(doc, reg)$nodes$n
  expect_identical(n$type, "models/pr_curve")
  expect_identical(reg$nodes[["models/pr_curve"]]$inputs$modelo$type, "models/fit")
})
