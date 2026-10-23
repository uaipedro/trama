# `tr_models_strip()` (reduzir.R): o modelo publicado prevê igual e não leva
# os dados de treino. Oráculo da previsão: o `predict()` do stats sobre o
# ajuste original, que é a referência do próprio R; tolerância 1e-12 (o mesmo
# produto matricial, só sem os campos que ele não lê).


test_that("lm reduzido prevê igual ao predict() do ajuste original, sem dado de treino", {
  d <- dados_reduzir()
  m <- tr_models_lm(d, formula = "y ~ poly(x1, 2) + x2 + trat")
  r <- tr_models_strip(m)
  expect_s3_class(r, "tr_models_lm")
  expect_true(r$reduzido)
  oraculo <- unname(stats::predict(m$ajuste, newdata = novos_reduzir))
  expect_equal(tr_models_predict_raw(r, novos_reduzir)$previsto, oraculo, tolerance = 1e-12)
  expect_equal(tr_models_predict_raw(r, novos_reduzir)$previsto,
               tr_models_predict_raw(m, novos_reduzir)$previsto, tolerance = 1e-12)
  # Sobrevive ao RDS, que é como sai no pacote.
  f <- tempfile(fileext = ".rds"); saveRDS(r, f)
  expect_equal(tr_models_predict_raw(readRDS(f), novos_reduzir)$previsto, oraculo, tolerance = 1e-12)

  # A varredura tem poder: no original ela acha o treino inteiro.
  expect_gte(length(vazados(m, c(d$x1, d$x2, d$y))), 3 * nrow(d))
  expect_length(vazados(r, c(d$x1, d$x2, d$y)), 0L)
  expect_false(nrow(d) %in% comprimentos_do_objeto(r))
  expect_null(r$dados)
  expect_identical(attr(r$ajuste$terms, ".Environment"), globalenv())
  expect_lt(length(serialize(r, NULL)), length(serialize(m, NULL)) / 4)

  i <- tr_models_info(r)
  expect_equal(i$n, nrow(d)); expect_equal(i$preditores, c("x1", "x2", "trat"))
  expect_error(tr_models_predict_raw(r, novos_reduzir, intervalo = "predicao"),
               class = "tr_models_error_not_applicable")
})

test_that("o ambiente da fórmula não leva a tabela junto", {
  # Um `lm` ajustado dentro de uma função guarda no `terms` o quadro dela,
  # com a tabela. Nos ajustes do trama o ambiente já é o global; o reduzido
  # garante isso para qualquer `lm`/`glm` que chegue.
  d <- dados_reduzir()
  ajustar <- function(dd) stats::lm(y ~ x1 + x2, data = dd)
  aj <- ajustar(d)
  expect_gt(length(vazados(attr(aj$terms, ".Environment"), d$x1)), 0L)
  r <- tr_models_strip_stats(aj)
  expect_identical(attr(r$terms, ".Environment"), globalenv())
  expect_length(vazados(r, c(d$x1, d$x2, d$y)), 0L)
  expect_lt(length(serialize(r, NULL)), length(serialize(d, NULL)))
})

test_that("glm Poisson reduzido prevê igual na escala da resposta", {
  d <- dados_reduzir()
  m <- tr_models_glm(d, formula = "cont ~ x1 + x2", familia = "poisson")
  r <- tr_models_strip(m)
  oraculo <- unname(stats::predict(m$ajuste, newdata = novos_reduzir, type = "response"))
  expect_equal(tr_models_predict_raw(r, novos_reduzir)$previsto, oraculo, tolerance = 1e-12)
  # Contagem tem valores pequenos que coincidem com posto e gl: a resposta
  # entra só pelo comprimento (nenhum vetor com uma posição por linha).
  expect_length(vazados(r, c(d$x1, d$x2)), 0L)
  expect_false(nrow(d) %in% comprimentos_do_objeto(r))
})

test_that("glm binomial de classificação reduzido mantém níveis, corte e probabilidades", {
  d <- dados_reduzir()
  m <- tr_models_glm(d, formula = "classe ~ x1 + x2 + trat", familia = "binomial")
  r <- tr_models_strip(m)
  expect_equal(tr_models_info(r)$tarefa, "classificacao")
  expect_equal(tr_models_info(r)$niveis, c("nao", "sim"))
  a <- tr_models_predict_raw(m, novos_reduzir); b <- tr_models_predict_raw(r, novos_reduzir)
  expect_identical(b$previsto, a$previsto)
  expect_equal(b$prob, a$prob, tolerance = 1e-12)
  expect_equal(unname(b$prob[, "sim"]),
               unname(stats::predict(m$ajuste, newdata = novos_reduzir, type = "response")), tolerance = 1e-12)
  expect_length(vazados(r, c(d$x1, d$x2, d$y)), 0L)
  expect_false(nrow(d) %in% comprimentos_do_objeto(r))
})

test_that("binomial negativa reduzida prevê igual", {
  skip_if_not_installed("MASS")
  d <- dados_reduzir()
  m <- tr_models_glm(d, formula = "cont ~ x1 + x2", familia = "binomial negativa")
  r <- tr_models_strip(m)
  oraculo <- unname(stats::predict(m$ajuste, newdata = novos_reduzir, type = "response"))
  expect_equal(tr_models_predict_raw(r, novos_reduzir)$previsto, oraculo, tolerance = 1e-12)
  expect_false(nrow(d) %in% comprimentos_do_objeto(r))
})

test_that("classe sem método não se reduz: erro com classe, sem default silencioso", {
  fake <- structure(list(dados = datasets::mtcars), class = c("tr_fake", "tr_models_fit"))
  expect_error(tr_models_strip(fake), class = "tr_models_error_no_method")
  expect_error(tr_models_strip(datasets::mtcars), class = "tr_models_error_no_method")
  # Misto ainda não tem reduzido: recusa, não publica a tabela.
  expect_error(tr_models_strip(milho_dbc() |> (\(x) { class(x) <- c("tr_models_lmer", "tr_models_fit"); x })()),
               class = "tr_models_error_no_method")
})

test_that("posto deficiente é recusado antes de publicar", {
  d <- dados_reduzir(); d$x3 <- 2 * d$x1
  expect_error(tr_models_strip(tr_models_lm(d, formula = "y ~ x1 + x3")),
               class = "tr_models_error_not_applicable")
})
