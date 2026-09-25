# Classificação pelos blocos da `trama.models`: a discriminante e a logística
# são `models/fit`, e prever, confundir e desenhar a ROC é com `models/predict`,
# `models/confusion` e `models/roc`, pelo contrato (`R/contrato.R`).
#
# Os números fixados aqui foram medidos com os nós ANTIGOS da multi
# (`multi/confusion`, `multi/roc`) antes de eles saírem, na Fase 4: a troca de
# casa não pode mudar uma taxa de acerto nem uma AUC de fluxo salvo.

pima <- function() tr_multi_example("pima")
vinhos4 <- "alcool, acidez_malica, magnesio, fenois_totais"

#' As AUCs de um `models/roc`, na ordem das classes (lidas da camada da curva).
auc_de <- function(p) {
  d <- Filter(function(l) "auc" %in% names(l$data), p$layers)[[1]]$data
  a <- tapply(d$auc, d$classe, `[`, 1)
  as.vector(a[unique(d$classe)])
}

modelos <- function() {
  list(lda_iris = tr_multi_discriminant(iris_t(), resposta = "Species"),
       qda_iris = tr_multi_discriminant(iris_t(), resposta = "Species", metodo = "quadrática"),
       lda_pima = tr_multi_discriminant(pima(), resposta = "diabetes"),
       logit_pima = tr_multi_logistic(pima(), resposta = "diabetes"),
       logit_vinhos = tr_multi_logistic(tr_multi_example("vinhos"), resposta = "cultivar", preditores = vinhos4),
       lda_cr = tr_multi_discriminant(tr_multi_example("caranguejos"), resposta = "grupo"))
}

# Medido com `tr_multi_confusion()` / `.tr_multi_roc_curva()` antes da Fase 4:
# acertos (a linha "total") e AUC de cada classe (uma só na binária).
antes <- list(
  lda_iris = list(cruzada = list(147L, c(1, 0.9972, 0.9972)),
                  "resubstituição" = list(147L, c(1, 0.9988, 0.9988))),
  qda_iris = list(cruzada = list(146L, c(1, 0.9986, 0.9986)),
                  "resubstituição" = list(147L, c(1, 0.9996, 0.9996))),
  lda_pima = list(cruzada = list(415L, 0.8499084905),
                  "resubstituição" = list(419L, 0.8595050529)),
  logit_pima = list(cruzada = list(414L, 0.8489695234),
                    "resubstituição" = list(419L, 0.8597437734)),
  logit_vinhos = list(cruzada = list(162L, c(0.9574134739, 0.9760431749, 0.9838141026)),
                      "resubstituição" = list(167L, c(0.9713715995, 0.9852573384, 0.9879807692))),
  lda_cr = list(cruzada = list(190L, c(0.9962666667, 0.9961333333, 1, 0.9996)),
                "resubstituição" = list(192L, c(0.9972, 0.9970666667, 1, 0.9997333333))))

test_that("confusão e ROC pela models dão os MESMOS números dos nós antigos da multi", {
  ms <- modelos()
  for (nm in names(ms)) for (va in names(antes[[nm]])) {
    esperado <- antes[[nm]][[va]]
    tab <- trama.models::tr_models_confusion(ms[[nm]], validacao = va)
    expect_equal(tab$acertos[nrow(tab)], esperado[[1]], info = paste(nm, va))
    expect_equal(auc_de(trama.models::tr_models_roc(ms[[nm]], validacao = va)), esperado[[2]],
                 tolerance = 1e-9, info = paste(nm, va))
  }
})

test_that("LOO da logística é o laço manual de glm sem a linha", {
  d <- pima()[1:80, ]
  m <- tr_multi_logistic(d, resposta = "diabetes", preditores = "glicose, imc")
  cv <- trama.models::tr_models_predict_cv(m, "cruzada")
  manual <- vapply(seq_len(nrow(d)), function(i) {
    f <- stats::glm(diabetes ~ glicose + imc, stats::binomial(), d[-i, ])
    unname(stats::predict(f, d[i, ], type = "response"))
  }, 0)
  expect_equal(unname(cv$prob[, "sim"]), manual, tolerance = 1e-8)
  expect_equal(levels(cv$previsto), c("não", "sim"))
})

test_that("LOO da LDA é o CV = TRUE da MASS, com as posteriores e os escores", {
  m <- tr_multi_discriminant(iris_t(), resposta = "Species")
  cv <- trama.models::tr_models_predict_cv(m, "cruzada")
  ref <- MASS::lda(as.matrix(datasets::iris[1:4]), datasets::iris$Species, CV = TRUE)
  expect_equal(unname(cv$prob), unname(ref$posterior))
  expect_identical(as.character(cv$previsto), as.character(ref$class))
  # Os escores LD saem mesmo na cruzada: são do modelo completo.
  expect_equal(names(cv$extra), c("LD1", "LD2"))
})

test_that("models/predict: treino por resubstituição ou cruzada, colunas previsto e prob_", {
  d <- pima()[1:80, ]
  m <- tr_multi_logistic(d, resposta = "diabetes", preditores = "glicose, imc")
  res <- trama.models::tr_models_predict(m)
  cru <- trama.models::tr_models_predict(m, validacao = "cruzada")
  expect_equal(names(cru), names(res))
  expect_equal(names(res), c(names(d), "previsto", "prob_não", "prob_sim"))
  expect_false(isTRUE(all.equal(res$prob_sim, cru$prob_sim)))
})

test_that("models/predict da LDA com dados novos: colunas, uma linha, faltante, reprever", {
  m <- tr_multi_discriminant(iris_t(), resposta = "Species")
  novos <- iris_t()[c(1, 51, 101), 1:4]
  cl <- trama.models::tr_models_predict(m, novos)
  expect_equal(as.character(cl$previsto), c("setosa", "versicolor", "virginica"))
  expect_equal(names(cl), c(names(novos), "previsto", "prob_setosa", "prob_versicolor",
                            "prob_virginica", "LD1", "LD2"))
  ref <- stats::predict(MASS::lda(as.matrix(datasets::iris[1:4]), datasets::iris$Species),
                        as.matrix(novos))
  expect_equal(unname(as.matrix(cl[6:8])), unname(ref$posterior))
  # Uma linha só também classifica (desvio padrão NA não é recusa aqui).
  expect_equal(nrow(trama.models::tr_models_predict(m, novos[1, ])), 1L)
  expect_error(trama.models::tr_models_predict(m, novos[, 1:2]), class = "tr_models_error_unknown_column")
  na <- novos; na$Petal.Width[2] <- NA
  expect_error(trama.models::tr_models_predict(m, na), class = "tr_multi_error_missing_values")
  txt <- novos; txt$Petal.Width <- as.character(txt$Petal.Width)
  expect_error(trama.models::tr_models_predict(m, txt), class = "tr_multi_error_not_numeric")
  # Reprever a saída substitui as colunas, em vez de duplicá-las.
  expect_equal(names(trama.models::tr_models_predict(m, cl)), names(cl))
  # A QDA não tem escores.
  q <- tr_multi_discriminant(iris_t(), resposta = "Species", metodo = "quadrática")
  expect_false("LD1" %in% names(trama.models::tr_models_predict(q)))
})

test_that("models/predict da logística multinomial com dados novos", {
  v <- tr_multi_example("vinhos")
  m <- tr_multi_logistic(v, resposta = "cultivar", preditores = vinhos4)
  cl <- trama.models::tr_models_predict(m, v[1:5, c("alcool", "acidez_malica", "magnesio", "fenois_totais")])
  expect_equal(nrow(cl), 5L)
  expect_equal(names(cl)[5:8], c("previsto", "prob_A", "prob_B", "prob_C"))
  expect_equal(rowSums(as.matrix(cl[6:8])), rep(1, 5), tolerance = 1e-8)
})

test_that("nomes de grupo com espaço viram prob_ citável; níveis vazios somem", {
  m <- tr_multi_discriminant(tr_multi_example("caranguejos"), resposta = "grupo")
  cl <- trama.models::tr_models_predict(m)
  expect_true(all(c("prob_azul_fêmea", "prob_laranja_macho") %in% names(cl)))
  expect_equal(ncol(trama.models::tr_models_confusion(m)), 1L + 4L + 3L)
  d <- iris_t()[1:100, ]
  m2 <- tr_multi_discriminant(d, resposta = "Species")
  expect_equal(levels(trama.models::tr_models_predict(m2)$previsto), c("setosa", "versicolor"))
  expect_equal(nrow(trama.models::tr_models_confusion(m2)), 3L)
})

test_that("a ROC binária marca o corte da logística, e a multiclasse segue a ordem dos níveis", {
  m <- tr_multi_logistic(pima(), resposta = "diabetes", corte = 0.3)
  p <- trama.models::tr_models_roc(m, validacao = "resubstituição")
  expect_match(p$labels$subtitle, "corte 0,30", fixed = TRUE)
  expect_no_warning(ggplot2::ggplot_build(p))
  d <- iris_t()
  d$Species <- factor(d$Species, levels = c("virginica", "setosa", "versicolor"))
  q <- trama.models::tr_models_roc(tr_multi_discriminant(d, resposta = "Species"))
  camadas <- Filter(function(ly) "grupo" %in% names(ly$data), q$layers)
  expect_equal(sub(" \\(.*$", "", levels(camadas[[1]]$data$grupo)),
               c("virginica", "setosa", "versicolor"))
})

test_that("motor: lda e logística chegam a models/confusion, roc e predict pela porta models/fit", {
  reg <- multi_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("p", "multi/example", dataset = "pima") |>
    trama::tr_add("lg", "multi/logistic", resposta = "diabetes", from = "p") |>
    trama::tr_add("ld", "multi/discriminant", resposta = "diabetes", from = "p") |>
    trama::tr_add("c1", "models/confusion", validacao = "resubstituição", from = "lg") |>
    trama::tr_add("c2", "models/confusion", from = "ld") |>
    trama::tr_add("r", "models/roc", validacao = "resubstituição", from = "lg") |>
    trama::tr_add("cabeca", "data/slice_head", n = 7L, from = "p") |>
    trama::tr_add("pr", "models/predict", from = "ld") |>
    trama::tr_link("cabeca", "pr:dados") |>
    trama::tr_add("pts", "view/points", x = "LD1", y = "glicose", cor = "previsto", from = "ld")
  expect_equal(rodar(f, "c1")$acertos[3], 419L)
  expect_equal(rodar(f, "c2")$acertos[3], 415L)
  expect_s3_class(rodar(f, "r"), "ggplot")
  expect_equal(nrow(rodar(f, "pr")), 7L)
  expect_s3_class(rodar(f, "pts"), "ggplot")
})
