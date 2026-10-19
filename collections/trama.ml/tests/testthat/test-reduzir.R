# `tr_models_strip()` dos motores da ml (contrato.R): previsão igual antes e
# depois e o que sobra do treino no objeto, motor a motor. Oráculo da
# previsão: o modelo original, pela mesma `tr_models_predict_raw()` (o mesmo
# `predict()` do motor sobre o mesmo objeto, sem os campos que ele não lê);
# tolerância zero — os números têm de ser idênticos.
#
# O que se mede de vazamento, com dados contínuos (rnorm/runif), onde acerto
# idêntico por acaso tem probabilidade desprezível:
# - linear e CART: nenhum valor do treino;
# - floresta (regressão): a resposta só reaparece como valor de folha;
# - SVM: os vetores de suporte são linhas do treino, e nada além deles;
# - XGBoost: a resposta não aparece; preditores só como corte de árvore.


ajustar_e_reduzir <- function(modelo, resposta) {
  d <- dados_ml_reduzir()
  m <- tr_ml_fit(d, resposta = resposta, preditores = "x1, x2, x3", modelo = modelo)
  list(d = d, m = m, r = trama.models::tr_models_strip(m))
}

expect_previsao_igual <- function(m, r) {
  a <- trama.models::tr_models_predict_raw(m, novos_ml)
  b <- trama.models::tr_models_predict_raw(r, novos_ml)
  expect_identical(b$previsto, a$previsto)
  expect_identical(b$prob, a$prob)
  # E depois do RDS, como sai no pacote (xgboost passa pelo UBJ).
  f <- tempfile(fileext = ".rds")
  saveRDS(trama.models::tr_models_serialize(r), f)
  c2 <- trama.models::tr_models_predict_raw(trama.models::tr_models_unserialize(readRDS(f)), novos_ml)
  expect_equal(c2$previsto, a$previsto, tolerance = 0)
  expect_equal(c2$prob, a$prob, tolerance = 0)
}

expect_sem_campos_de_treino <- function(r) {
  expect_true(r$reduzido)
  expect_null(r$dados); expect_null(r$treino_impressoes); expect_null(r$origem)
  i <- trama.models::tr_models_info(r)
  expect_equal(i$n, 120L); expect_equal(i$preditores, c("x1", "x2", "x3"))
}

for (resp in c("y", "cl")) {
  test_that(sprintf("linear (%s) reduzido: previsão idêntica, nenhum valor do treino", resp), {
    z <- ajustar_e_reduzir("linear", resp)
    expect_previsao_igual(z$m, z$r)
    expect_sem_campos_de_treino(z$r)
    expect_gt(length(vazados(z$m, c(z$d$x1, z$d$x2, z$d$x3))), 0L)  # a varredura enxerga o original
    expect_length(vazados(z$r, c(z$d$x1, z$d$x2, z$d$x3, z$d$y)), 0L)
    expect_false(120L %in% comprimentos_do_objeto(z$r$ajuste))
  })
}

for (resp in c("y", "cl", "c3")) {
  test_that(sprintf("CART (%s) reduzido: previsão idêntica, nenhum valor do treino", resp), {
    z <- ajustar_e_reduzir("cart", resp)
    expect_previsao_igual(z$m, z$r)
    expect_sem_campos_de_treino(z$r)
    expect_gt(length(vazados(z$m, c(z$d$x1, z$d$x2, z$d$x3))), 0L)
    expect_length(vazados(z$r, c(z$d$x1, z$d$x2, z$d$x3, z$d$y)), 0L)
    expect_null(z$r$ajuste$y); expect_null(z$r$ajuste$where)
  })
}

test_that("floresta reduzida: previsão idêntica; a resposta só sobra como valor de folha", {
  skip_if_not_installed("ranger")
  for (resp in c("y", "cl", "c3")) {
    z <- ajustar_e_reduzir("forest", resp)
    expect_previsao_igual(z$m, z$r)
    expect_sem_campos_de_treino(z$r)
    expect_null(z$r$ajuste$predictions)
    expect_length(vazados(z$r, c(z$d$x1, z$d$x2, z$d$x3)), 0L)
  }
  # Regressão: o `ranger` guarda o valor da folha em `split.values` dos nós
  # terminais (filhos 0 e 0). Folha com uma única linha distinta sorteada
  # guarda a resposta dela — é o modelo, não um campo a mais.
  z <- ajustar_e_reduzir("forest", "y")
  fo <- z$r$ajuste$forest
  folhas <- unlist(lapply(seq_len(fo$num.trees), function(k) {
    filhos <- fo$child.nodeIDs[[k]]
    fo$split.values[[k]][filhos[[1]] == 0 & filhos[[2]] == 0]
  }))
  vaz <- vazados(z$r, z$d$y)
  expect_gt(length(vaz), 0L)
  expect_true(all(vaz %in% folhas))
})

test_that("SVM reduzida: previsão idêntica; sobram só os vetores de suporte, que são linhas do treino", {
  skip_if_not_installed("e1071")
  for (resp in c("y", "cl", "c3")) {
    z <- ajustar_e_reduzir("svm", resp)
    expect_previsao_igual(z$m, z$r)
    expect_sem_campos_de_treino(z$r)
    aj <- z$r$ajuste
    for (campo in c("fitted", "residuals", "decision.values", "index", "call")) expect_null(aj[[campo]])
    # Desfeita a escala, cada vetor de suporte é uma linha do treino.
    sv <- t(t(aj$SV) * aj$x.scale[["scaled:scale"]] + aj$x.scale[["scaled:center"]])
    treino <- round(as.matrix(z$d[c("x1", "x2", "x3")]), 8)
    linhas <- apply(round(sv, 8), 1, function(l) any(apply(treino, 1, function(q) all(q == l))))
    expect_true(all(linhas))
    expect_equal(nrow(sv), aj$tot.nSV)
    # E nada fora deles: sem a matriz SV, nenhum valor (cru) do treino.
    aj$SV <- NULL
    expect_length(vazados(aj, c(z$d$x1, z$d$x2, z$d$x3, z$d$y)), 0L)
  }
})

test_that("XGBoost reduzido: previsão idêntica; a resposta não aparece, preditores só como corte", {
  skip_if_not_installed("xgboost")
  for (resp in c("y", "cl", "c3")) {
    z <- ajustar_e_reduzir("xgboost", resp)
    expect_previsao_igual(z$m, z$r)
    expect_sem_campos_de_treino(z$r)
    expect_null(attr(z$r$ajuste, "call"))
    j <- jsonlite::fromJSON(rawToChar(xgboost::xgb.save.raw(z$r$ajuste, raw_format = "json")),
                            simplifyVector = FALSE)
    arvores <- j$learner$gradient_booster$model$trees
    cortes <- unlist(lapply(arvores, function(a) unlist(a$split_conditions)))
    folha <- unlist(lapply(arvores, function(a) unlist(a$base_weights)))
    # O booster guarda float32: a comparação é com tolerância de float32.
    perto <- function(v, alvo) vapply(alvo, function(t) any(abs(v - t) <= 1e-6 * abs(t)), TRUE)
    numeros <- c(cortes, folha)
    expect_false(any(perto(numeros, z$d$y)))
    x <- c(z$d$x1, z$d$x2, z$d$x3)
    # Os acertos em preditores vêm todos dos cortes (quantis do histograma).
    expect_identical(sum(perto(numeros, x)), sum(perto(cortes, x)))
  }
})

test_that("FIGS não se reduz: erro de contrato, com o motivo", {
  skip_if_not_installed("figsr")
  d <- dados_ml_reduzir()
  m <- tr_ml_fit(d, resposta = "y", preditores = "x1, x2, x3", modelo = "figs")
  expect_error(trama.models::tr_models_strip(m), class = "tr_models_error_no_method")
})
