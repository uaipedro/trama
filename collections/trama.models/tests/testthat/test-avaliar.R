# Os blocos que medem a previsão (confusion, roc, evaluate, importance) e o
# predict ampliado. O oráculo são as funções da `multi` e da `ml` que eles vão
# substituir, ainda vivas nesta fase: mesmos dados, mesma validação, mesmo
# número. Sem elas instaladas, os testes de oráculo pulam.

mt <- ex("mtcars")
mt$am_f <- factor(mt$am)
logit <- function() tr_models_glm(mt, formula = "am_f ~ wt + hp", familia = "binomial")
subtitulo <- function(p) p$labels$subtitle

# ---- glm binomial é classificação ------------------------------------------

test_that("glm binomial: níveis pela resposta (fator, 0/1, lógica); proporção segue regressão", {
  expect_equal(tr_models_info(logit())$niveis, c("0", "1"))
  expect_equal(tr_models_info(tr_models_glm(mt, formula = "am ~ wt", familia = "binomial"))$niveis, c("0", "1"))
  d <- mt; d$am_l <- d$am == 1
  il <- tr_models_info(tr_models_glm(d, formula = "am_l ~ wt", familia = "binomial"))
  expect_equal(il$tarefa, "classificacao"); expect_equal(il$niveis, c("FALSE", "TRUE"))
  d$prop <- d$am * 0.5 + 0.25
  ip <- tr_models_info(suppressWarnings(tr_models_glm(d, formula = "prop ~ wt", familia = "binomial")))
  expect_equal(ip$tarefa, "regressao")
})

test_that("predict_cv cruzada do glm confere com o deixa-um-fora feito à mão", {
  g <- logit()
  loo <- vapply(seq_len(nrow(mt)), function(i) {
    unname(predict(glm(am_f ~ wt + hp, binomial, data = mt[-i, ]), newdata = mt[i, ], type = "response"))
  }, numeric(1))
  cv <- tr_models_predict_cv(g, "cruzada")
  expect_equal(unname(cv$prob[, "1"]), loo)
  expect_equal(unname(cv$prob[, "0"]), 1 - loo)
})

# ---- models/predict ------------------------------------------------------------

test_that("predict sem dados: treino por resubstituição (padrão) ou cruzada, com prob_*", {
  g <- logit()
  r <- tr_models_predict(g)
  expect_equal(nrow(r), nrow(mt))
  expect_equal(r$prob_1, unname(fitted(g$ajuste)))
  expect_s3_class(r$previsto, "factor")
  c <- tr_models_predict(g, validacao = "cruzada")
  expect_equal(c$prob_1, unname(tr_models_predict_cv(g, "cruzada")$prob[, "1"]))
  # lm: cruzada é o PRESS; resubstituição com intervalo é o predict() no treino
  m <- tr_models_lm(mt, formula = "mpg ~ wt")
  expect_equal(tr_models_predict(m, validacao = "cruzada")$previsto,
               unname(mt$mpg - residuals(m$ajuste) / (1 - hatvalues(m$ajuste))))
  ic <- tr_models_predict(m, intervalo = "confianca")
  expect_equal(ic$li, unname(predict(m$ajuste, interval = "confidence")[, "lwr"]))
  expect_error(tr_models_predict(m, intervalo = "confianca", validacao = "cruzada"),
               class = "tr_models_error_not_applicable")
})

test_that("predict: prever a saída de outra previsão substitui as colunas", {
  g <- logit()
  r2 <- tr_models_predict(g, tr_models_predict(g, mt))
  expect_equal(sum(names(r2) == "previsto"), 1L)
  expect_equal(sum(startsWith(names(r2), "prob_")), 2L)
})

# ---- models/confusion ------------------------------------------------------------

test_that("confusion nos três modos bate com tr_multi_confusion (mesma validação)", {
  skip_if_not_installed("trama.multi")
  g <- logit()
  lg <- trama.multi::tr_multi_logistic(mt, resposta = "am_f", preditores = "wt, hp")
  for (v in c("cruzada", "resubstituição")) {
    ref <- as.data.frame(trama.multi::tr_multi_confusion(lg, v))
    expect_equal(as.data.frame(tr_models_confusion(g, validacao = v)), ref, info = v)
    # modo tabela, sobre a saída do models/predict com a mesma validação
    tab <- tr_models_predict(g, validacao = v)
    expect_equal(as.data.frame(tr_models_confusion(dados = tab, resposta = "am_f")), ref, info = v)
  }
  # modelo + dados: prever o próprio treino é a resubstituição
  expect_equal(as.data.frame(tr_models_confusion(g, mt)),
               as.data.frame(trama.multi::tr_multi_confusion(lg, "resubstituição")))
})

test_that("confusion no modo tabela conta como a tr_ml_confusion (formato largo x longo)", {
  skip_if_not_installed("trama.ml")
  d <- data.frame(y = c("a", "a", "b", "c", "c", "c"), previsto = c("a", "b", "b", "c", "a", "c"))
  larga <- tr_models_confusion(dados = d, resposta = "y")
  longa <- trama.ml::tr_ml_confusion(d, resposta = "y", predito = "previsto")
  for (i in seq_len(nrow(longa))) {
    expect_equal(larga[[longa$previsto[[i]]]][larga$real == longa$observado[[i]]], longa$n[[i]])
  }
  expect_equal(larga$taxa_acerto[larga$real == "total"], 4 / 6)
})

test_that("confusion recusa: sem entrada, regressão, coluna que falta", {
  expect_error(tr_models_confusion(), class = "tr_models_error_no_input")
  expect_error(tr_models_roc(), class = "tr_models_error_no_input")
  expect_error(tr_models_evaluate(), class = "tr_models_error_no_input")
  expect_error(tr_models_confusion(tr_models_lm(mt, formula = "mpg ~ wt")), class = "tr_models_error_not_applicable")
  expect_error(tr_models_confusion(logit(), mt[, c("wt", "hp")]), class = "tr_models_error_unknown_column")
  expect_error(tr_models_confusion(dados = mt, resposta = "am", predito = "nada"),
               class = "tr_models_error_unknown_column")
})

# ---- models/roc ----------------------------------------------------------------

test_that("roc: a AUC e o corte batem com tr_multi_roc e tr_ml_roc", {
  skip_if_not_installed("trama.multi")
  skip_if_not_installed("trama.ml")
  g <- logit()
  lg <- trama.multi::tr_multi_logistic(mt, resposta = "am_f", preditores = "wt, hp")
  for (v in c("cruzada", "resubstituição")) {
    expect_equal(subtitulo(tr_models_roc(g, validacao = v)), subtitulo(trama.multi::tr_multi_roc(lg, v)))
  }
  tab <- tr_models_predict(g, validacao = "cruzada")
  auc_ml <- ggplot2::ggplot_build(trama.ml::tr_ml_roc(tab, resposta = "am_f", probabilidade = "prob_1",
                                                      positiva = "1"))$plot$data$auc[[1]]
  rd <- trama.models:::.tr_models_roc_dados(as.character(tab$am_f), cbind(`0` = tab$prob_0, `1` = tab$prob_1),
                                            c("0", "1"))
  expect_equal(rd$curvas$auc[[1]], auc_ml)
  # modo tabela (probabilidade vazia = prob_<positiva>) e modelo + dados
  expect_match(subtitulo(tr_models_roc(dados = tab, resposta = "am_f")), "AUC 0,943 · tabela", fixed = TRUE)
  expect_match(subtitulo(tr_models_roc(g, mt)), "dados novos", fixed = TRUE)
  expect_match(subtitulo(tr_models_roc(g, positiva = "0")), "positivo: 0", fixed = TRUE)
})

test_that("roc marca o corte do modelo quando ele traz um", {
  g <- logit(); g$corte <- 0.3
  expect_match(subtitulo(tr_models_roc(g)), "corte 0,30", fixed = TRUE)
})

test_that("roc multiclasse (modo tabela): uma curva por classe, AUCs da tr_multi_roc", {
  skip_if_not_installed("trama.multi")
  ir <- tibble::as_tibble(datasets::iris)
  lda <- trama.multi::tr_multi_discriminant(ir, resposta = "Species")
  tab <- trama.multi::tr_multi_classify(lda, validacao = "resubstituição")
  p <- tr_models_roc(dados = tab, resposta = "Species")
  ref <- trama.multi::tr_multi_roc(lda, "resubstituição")
  expect_equal(levels(p$data$grupo %||% ggplot2::ggplot_build(p)$plot$layers[[2]]$data$grupo),
               levels(ggplot2::ggplot_build(ref)$plot$layers[[2]]$data$grupo))
  expect_error(tr_models_roc(dados = tab, resposta = "Species", probabilidade = "prob_setosa"),
               class = "tr_models_error_blank_param")
})

test_that("roc recusa regressão", {
  expect_error(tr_models_roc(tr_models_lm(mt, formula = "mpg ~ wt")), class = "tr_models_error_not_applicable")
})

# ---- models/evaluate -------------------------------------------------------------

test_that("evaluate: as métricas batem com tr_ml_evaluate nos três modos", {
  skip_if_not_installed("trama.ml")
  g <- logit()
  tab <- tr_models_predict(g, validacao = "cruzada")
  ref <- trama.ml::tr_ml_evaluate(tab, resposta = "am_f", predito = "previsto")
  e <- tr_models_evaluate(g)
  expect_equal(e[match(ref$metrica, e$metrica), ], ref)
  expect_equal(tr_models_evaluate(dados = tab, resposta = "am_f"), e)
  # kappa, sensibilidade, especificidade conferidas contra a matriz
  cm <- table(tab$am_f, tab$previsto)
  po <- sum(diag(cm)) / sum(cm); pe <- sum(rowSums(cm) * colSums(cm)) / sum(cm)^2
  expect_equal(e$valor[e$metrica == "kappa"], (po - pe) / (1 - pe))
  expect_equal(e$valor[e$metrica == "sensitivity"], cm["1", "1"] / sum(cm["1", ]))
  expect_equal(e$valor[e$metrica == "specificity"], cm["0", "0"] / sum(cm["0", ]))

  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  tr <- tr_models_predict(m, validacao = "cruzada")
  expect_equal(tr_models_evaluate(m), trama.ml::tr_ml_evaluate(tr, resposta = "mpg", predito = "previsto"))
  expect_equal(tr_models_evaluate(m, mt), trama.ml::tr_ml_evaluate(tr_models_predict(m, mt), resposta = "mpg",
                                                                   predito = "previsto"))
})

# ---- models/importance / coefficients ---------------------------------------------

test_that("importance: a tabela do contrato; recusa no misto", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  expect_identical(tr_models_importance_table(m), tr_models_importance(m))
  expect_named(tr_models_importance_table(m), c("termo", "importancia", "medida"))
  misto <- tr_models_lmer(ex("sleepstudy"), formula = "Reaction ~ Days + (1 | Subject)")
  expect_error(tr_models_importance_table(misto), class = "tr_models_error_not_applicable")
})

test_that("coefficients: confiança no nome e no intervalo; escala DP multiplica e não muda o p", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  c90 <- tr_models_coefficients(m, confianca = 0.9)$tabela
  expect_equal(c90$li_90, unname(confint(m$ajuste, level = 0.9)[, 1]))
  expect_true(all(c("li_95", "ls_95") %in% names(tr_models_coefficients(m)$tabela)))
  u <- tr_models_coefficients(m)$tabela
  s <- tr_models_coefficients(m, escala = "desvio padrão")$tabela
  expect_equal(s$estimativa[-1], u$estimativa[-1] * c(sd(mt$wt), sd(mt$hp)))
  expect_equal(s$p_valor, u$p_valor)
  # glm: exponenciado por DP = exp(b · DP), como a razão de chances padronizada da multi
  g <- logit()
  e <- tr_models_coefficients(g, exponenciar = TRUE, escala = "desvio padrão")$tabela
  expect_equal(e$estimativa[2], exp(coef(g$ajuste)[["wt"]] * sd(mt$wt)))
  expect_error(tr_models_coefficients(m, escala = "outra"), class = "tr_models_error_bad_option")
})

test_that("coefficients: batem com tr_multi_logistic_coefficients (escala e confiança)", {
  skip_if_not_installed("trama.multi")
  g <- logit()
  lg <- trama.multi::tr_multi_logistic(mt, resposta = "am_f", preditores = "wt, hp")
  for (esc in c("unidade", "desvio padrão")) {
    ref <- trama.multi::tr_multi_logistic_coefficients(lg, escala = esc, confianca = 0.9)
    t <- tr_models_coefficients(g, exponenciar = TRUE, escala = esc, confianca = 0.9)$tabela
    expect_equal(t$estimativa, ref$razao_chances, tolerance = 1e-6, info = esc)
    expect_equal(t$li_90, ref$ic_inf, tolerance = 1e-6, info = esc)
    expect_equal(t$p_valor, ref$p_valor, tolerance = 1e-6, info = esc)
  }
})

# ---- pelo motor ----------------------------------------------------------------------

test_that("pelo motor: entradas opcionais chegam como NULL nos três modos", {
  reg <- models_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("carros", "models/example", dataset = "mtcars") |>
    trama::tr_add("lg", "models/glm", formula = "am ~ wt", familia = "binomial", from = "carros") |>
    trama::tr_add("so_modelo", "models/confusion", from = "lg") |>
    trama::tr_add("os_dois", "models/evaluate", from = c("lg", "carros")) |>
    trama::tr_add("prev", "models/predict", validacao = "cruzada", from = "lg") |>
    trama::tr_add("tabela", "models/confusion", resposta = "am", from = list(dados = "prev"))
  expect_equal(rodar(f, "so_modelo")$real, c("0", "1", "total"))
  expect_equal(rodar(f, "tabela"), rodar(f, "so_modelo"))
  expect_true("accuracy" %in% rodar(f, "os_dois")$metrica)
})
