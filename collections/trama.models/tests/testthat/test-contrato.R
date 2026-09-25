# O contrato de modelo (contrato.R): o store aceita qualquer classe que o
# implemente, RDS antigo continua lendo, e a cruzada do lm é o LOO de verdade.

mt <- datasets::mtcars

# Um round-trip pelo store REAL do núcleo, com o tipo da coleção: é o caminho
# que o motor faz entre dois nós.
ida_e_volta <- function(x) {
  s <- trama::tr_store(tempfile())
  h <- trama::tr_store_put(s, "k", x, models_fit_type())
  list(valor = trama::tr_store_get(s, "k", models_fit_type()), handle = h)
}

test_that("as classes da models saem com subclasse e cumprem o contrato", {
  expect_equal(class(milho_dbc()), c("tr_models_lm", "tr_models_fit"))
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  expect_equal(class(g), c("tr_models_glm", "tr_models_fit"))
  i <- tr_models_info(g)
  # Binomial de resposta 0/1 é classificação desde a T2 da Fase 4.
  expect_equal(i$tarefa, "classificacao"); expect_equal(i$niveis, c("0", "1"))
  expect_equal(i$familia, "binomial")
  expect_equal(i$preditores, "wt"); expect_equal(i$n, nrow(mt))
  expect_identical(tr_models_stats(milho_dbc()), tr_models_fit_stats(milho_dbc()))
})

test_that("classe nova só com os métodos do contrato passa pelo store e pelos genéricos", {
  fake <- structure(list(media = 20, resposta = "mpg"), class = c("tr_fake", "tr_models_fit"))
  local_mocked_s3_method("tr_models_info", "tr_fake", function(x) {
    list(tarefa = "regressao", resposta = x$resposta, preditores = "wt", niveis = NULL,
         n = 32L, rotulo = "Falso", familia = NULL)
  })
  local_mocked_s3_method("tr_models_predict_raw", "tr_fake", function(x, novos, ...) {
    list(previsto = rep(x$media, nrow(novos)), prob = NULL, extra = NULL)
  })
  local_mocked_s3_method("tr_models_stats", "tr_fake", function(x) tibble::tibble(modelo = "Falso", n = 32L))
  local_mocked_s3_method("tr_models_card", "tr_fake", function(x, ctx) {
    trama::tr_preview("models/fit", data = list(rotulo = "Falso"))
  })

  r <- ida_e_volta(fake)
  expect_equal(r$valor, fake)
  expect_equal(r$handle$preview$data$rotulo, "Falso")
  expect_equal(tr_models_fit_stats(r$valor)$modelo, "Falso")
  p <- tr_models_predict(r$valor, mt[1:3, ])
  expect_equal(p$previsto, rep(20, 3))
  # O que a classe não implementou vira erro com classe, nomeando-a.
  expect_error(tr_models_as_table(fake), class = "tr_models_error_no_method")
  expect_error(tr_models_coefficients(fake), "tr_fake", class = "tr_models_error_no_method")
})

test_that("info fora do contrato é recusado pelo store", {
  local_mocked_s3_method("tr_models_info", "tr_torto", function(x) list(tarefa = "classificacao"))
  torto <- structure(list(), class = c("tr_torto", "tr_models_fit"))
  expect_error(trama::tr_store_put(trama::tr_store(tempfile()), "k", torto, models_fit_type()),
               class = "tr_models_error_bad_info")
})

test_that("RDS antigo, só com 'tr_models_fit', continua lendo e funcionando nos leitores", {
  novo <- milho_dbc()
  antigo <- unclass(novo); class(antigo) <- "tr_models_fit"
  s <- trama::tr_store(tempfile())
  h <- trama::tr_store_put(s, "k", antigo, models_fit_type())   # grava como o código antigo gravaria
  f <- list.files(s$root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  saveRDS(antigo, f[[1]], compress = FALSE)
  lido <- trama::tr_store_get(s, "k", models_fit_type())
  expect_equal(class(lido), class(novo))
  expect_equal(tr_models_anova_table(lido)$tabela, tr_models_anova_table(novo)$tabela)
  expect_equal(tr_models_emmeans(lido, "hibrido")$tabela, tr_models_emmeans(novo, "hibrido")$tabela)
  expect_equal(tr_models_as_table(lido), tr_models_as_table(novo))
  # Em memória, sem passar pelo store: o porteiro promove e redespacha.
  expect_equal(tr_models_fit_stats(antigo), tr_models_fit_stats(novo))
  expect_equal(tr_models_coefficients(antigo)$tabela, tr_models_coefficients(novo)$tabela)
  expect_equal(tr_models_residuals(antigo), tr_models_residuals(novo))
})

test_that("predict_cv: resubstituição é o ajustado; a cruzada do lm é o LOO à mão", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  expect_equal(tr_models_predict_cv(m)$previsto, unname(fitted(m$ajuste)))
  loo <- vapply(seq_len(nrow(mt)), function(i) {
    unname(predict(lm(mpg ~ wt + hp, data = mt[-i, ]), newdata = mt[i, ]))
  }, numeric(1))
  expect_equal(tr_models_predict_cv(m, "cruzada")$previsto, loo)
  # O delineamento (um `aov`) pela mesma fórmula fechada.
  d <- milho_dbc()$dados
  loo_d <- vapply(seq_len(nrow(d)), function(i) {
    unname(predict(lm(producao ~ bloco + hibrido, data = d[-i, ]), newdata = d[i, ]))
  }, numeric(1))
  expect_equal(tr_models_predict_cv(milho_dbc(), "cruzada")$previsto, loo_d)
  g <- tr_models_glm(mt, formula = "am ~ wt", familia = "binomial")
  loo_g <- vapply(seq_len(nrow(mt)), function(i) {
    unname(predict(glm(am ~ wt, binomial, data = mt[-i, ]), newdata = mt[i, ], type = "response"))
  }, numeric(1))
  cv <- tr_models_predict_cv(g, "cruzada")
  expect_equal(unname(cv$prob[, "1"]), loo_g)
  expect_equal(cv$previsto, factor(ifelse(loo_g >= 0.5, "1", "0"), levels = c("0", "1")))
  expect_error(tr_models_predict_cv(m, "outra"), class = "tr_models_error_bad_option")
})

test_that("importance: |t| no lm, recusada no misto", {
  m <- tr_models_lm(mt, formula = "mpg ~ wt + hp")
  imp <- tr_models_importance(m)
  expect_equal(imp$termo, c("wt", "hp"))
  expect_equal(imp$importancia, unname(abs(summary(m$ajuste)$coefficients[c("wt", "hp"), "t value"])))
  expect_equal(unique(imp$medida), "|t|")
  mm <- tr_models_lmer(lme4::sleepstudy, formula = "Reaction ~ Days + (1 | Subject)")
  expect_error(tr_models_importance(mm), class = "tr_models_error_not_applicable")
})

test_that("restaurar subclasse sem método registrado diz qual coleção carregar", {
  s <- trama::tr_store(tempfile())
  h <- trama::tr_store_put(s, "k", milho_dbc(), models_fit_type())
  f <- list.files(s$root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  saveRDS(structure(list(), class = c("tr_ml_fit", "tr_models_fit")), f[[1]])
  expect_error(trama::tr_store_get(s, "k", models_fit_type()), "trama\\.ml",
               class = "tr_models_error_no_method")
})

test_that("tr_models_clean_name saneia níveis para prob_<nivel>", {
  expect_equal(tr_models_clean_name(c("Iris setosa", "não-germinou", "a b", "a-b", "")),
               c("Iris_setosa", "não_germinou", "a_b", "a_b_1", "grupo"))
})
