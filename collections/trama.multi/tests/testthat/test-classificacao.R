# Classificação comum aos dois modelos: previsão, deixa-um-fora, confusão e ROC.

pima <- function() tr_multi_example("pima")

test_that("LOO da logística é o laço manual de glm sem a linha", {
  d <- pima()[1:80, ]
  m <- tr_multi_logistic(d, grupo = "diabetes", cols = "glicose, imc")
  cv <- .tr_multi_prever(m, "cruzada", "x")
  manual <- vapply(seq_len(nrow(d)), function(i) {
    f <- stats::glm(diabetes ~ glicose + imc, stats::binomial(), d[-i, ])
    unname(stats::predict(f, d[i, ], type = "response"))
  }, 0)
  expect_equal(unname(cv$prob[, "sim"]), manual, tolerance = 1e-8)
  expect_equal(levels(cv$classe), c("não", "sim"))
})

test_that("LOO da LDA é o CV = TRUE da MASS, com as posteriores", {
  m <- tr_multi_discriminant(iris_t(), grupo = "Species")
  cv <- .tr_multi_prever(m, "cruzada", "x")
  ref <- MASS::lda(as.matrix(datasets::iris[1:4]), datasets::iris$Species, CV = TRUE)
  expect_equal(unname(cv$prob), unname(ref$posterior))
  expect_identical(as.character(cv$classe), as.character(ref$class))
})

test_that("classify com validação cruzada devolve as probabilidades sem a linha", {
  d <- pima()[1:80, ]
  m <- tr_multi_logistic(d, grupo = "diabetes", cols = "glicose, imc")
  res <- tr_multi_classify(m)
  cru <- tr_multi_classify(m, validacao = "cruzada")
  expect_equal(names(cru), names(res))
  expect_equal(names(res), c(names(d), "previsto", "prob_não", "prob_sim"))
  expect_false(isTRUE(all.equal(res$prob_sim, cru$prob_sim)))
  expect_error(tr_multi_classify(m, novos = d[1:3, ], validacao = "cruzada"),
               class = "tr_multi_error_bad_option")
})

test_that("confusion aceita a logística, e a da LDA não mudou", {
  m <- tr_multi_logistic(pima(), grupo = "diabetes")
  tab <- tr_multi_confusion(m)
  expect_equal(tab$real, c("não", "sim", "total"))
  expect_gt(tab$taxa_acerto[3], .7)
  expect_lt(tab$taxa_acerto[3], .85)
  res <- tr_multi_confusion(m, "resubstituição")
  expect_gte(res$acertos[3], tab$acertos[3] - 5L)
  expect_error(tr_multi_confusion(list(a = 1)), class = "tr_multi_error_not_a_classifier")
})

test_that("classify da logística multinomial com novos", {
  v <- tr_multi_example("vinhos")
  m <- tr_multi_logistic(v, grupo = "cultivar", cols = "alcool, acidez_malica, magnesio, fenois_totais")
  cl <- tr_multi_classify(m, novos = v[1:5, c("alcool", "acidez_malica", "magnesio", "fenois_totais")])
  expect_equal(nrow(cl), 5L)
  expect_equal(names(cl)[5:8], c("previsto", "prob_A", "prob_B", "prob_C"))
  expect_equal(rowSums(as.matrix(cl[6:8])), rep(1, 5), tolerance = 1e-8)
})

test_that("AUC é a estatística de Mann-Whitney normalizada, com empates", {
  score <- c(.1, .4, .35, .8, .8, .2)
  pos <- c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
  cur <- .tr_multi_roc_curva(score, pos)
  w <- stats::wilcox.test(score[pos], score[!pos], exact = FALSE)$statistic
  expect_equal(cur$auc, unname(w) / (sum(pos) * sum(!pos)))
  expect_equal(cur$pontos$fpr[1], 0); expect_equal(cur$pontos$tpr[1], 0)
  expect_equal(utils::tail(cur$pontos$fpr, 1), 1); expect_equal(utils::tail(cur$pontos$tpr, 1), 1)
  expect_true(all(diff(cur$pontos$fpr) >= 0))
  # A área trapezoidal da curva (empates na diagonal) é a própria AUC.
  trap <- sum(diff(cur$pontos$fpr) * (utils::head(cur$pontos$tpr, -1) + cur$pontos$tpr[-1]) / 2)
  expect_equal(trap, cur$auc)
})

test_that("roc binária da logística e multiclasse da LDA desenham", {
  m <- tr_multi_logistic(pima(), grupo = "diabetes")
  p <- tr_multi_roc(m, validacao = "resubstituição")
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "AUC", fixed = TRUE)
  expect_no_warning(ggplot2::ggplot_build(p))
  l <- tr_multi_discriminant(iris_t(), grupo = "Species")
  q <- tr_multi_roc(l)
  b <- ggplot2::ggplot_build(q)
  expect_equal(length(unique(b$data[[2]]$group)), 3L)
})

test_that("roc multiclasse: a legenda segue a ordem dos níveis, e não a alfabética", {
  d <- iris_t()
  d$Species <- factor(d$Species, levels = c("virginica", "setosa", "versicolor"))
  q <- tr_multi_roc(tr_multi_discriminant(d, grupo = "Species"))
  camadas <- Filter(function(ly) "grupo" %in% names(ly$data), q$layers)
  expect_length(camadas, 1L)
  expect_equal(sub(" \\(.*$", "", levels(camadas[[1]]$data$grupo)),
               c("virginica", "setosa", "versicolor"))
})

test_that("motor: lda e logística chegam a confusion e roc pelo adaptador", {
  reg <- multi_registry()
  expect_true(trama::tr_compatible("multi/lda", "multi/classifier", reg))
  expect_true(trama::tr_compatible("multi/logit", "multi/classifier", reg))
  f <- trama::tr_flow(reg) |>
    trama::tr_add("p", "multi/example", dataset = "pima") |>
    trama::tr_add("lg", "multi/logistic", grupo = "diabetes", from = "p") |>
    trama::tr_add("ld", "multi/discriminant", grupo = "diabetes", from = "p") |>
    trama::tr_add("c1", "multi/confusion", validacao = "resubstituição", from = "lg") |>
    trama::tr_add("c2", "multi/confusion", from = "ld") |>
    trama::tr_add("r", "multi/roc", validacao = "resubstituição", from = "lg")
  expect_equal(rodar(f, "c1")$real, c("não", "sim", "total"))
  expect_equal(rodar(f, "c2")$real, c("não", "sim", "total"))
  expect_s3_class(rodar(f, "r"), "ggplot")
})
