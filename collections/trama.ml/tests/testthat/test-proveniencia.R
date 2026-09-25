# Isolamento do teste imposto pela proveniência que o `ml/split` grava
# (Kaufman et al. 2012, doi:10.1145/2382577.2382579: vazamento treino-teste).

binaria <- function() tr_ml_example("iris_binaria")

test_that("split marca treino e teste com o mesmo id, reprodutível", {
  s <- tr_ml_split(binaria(), "Species", seed = 3)
  ot <- attr(s$treino, "tr_ml_origem"); oe <- attr(s$teste, "tr_ml_origem")
  expect_equal(ot$papel, "treino"); expect_equal(oe$papel, "teste")
  expect_identical(ot$divisao, oe$divisao)
  expect_identical(attr(tr_ml_split(binaria(), "Species", seed = 3)$teste, "tr_ml_origem"), oe)
  expect_false(identical(attr(tr_ml_split(binaria(), "Species", seed = 4)$teste, "tr_ml_origem")$divisao,
                         oe$divisao))
  for (est in c("temporal", "grupo")) {
    d <- tibble::tibble(t = 1:20, g = rep(letters[1:5], each = 4), y = rnorm(20), x = 1:20)
    z <- tr_ml_split(d, "y", estrategia = est, ordem = "t", grupo = "g")
    expect_equal(attr(z$teste, "tr_ml_origem")$papel, "teste", info = est)
  }
  # os dados continuam os mesmos: só o atributo foi acrescentado
  expect_equal(nrow(s$treino) + nrow(s$teste), 100L)
  expect_equal(names(s$teste), names(binaria()))
})

test_that("ajustar no teste é recusado em todos os blocos de ajuste", {
  s <- tr_ml_split(binaria(), "Species")
  expect_error(tr_ml_cart(s$teste, "Species"), class = "tr_ml_error_test_leak")
  expect_error(tr_ml_linear(s$teste, "Species"), class = "tr_ml_error_test_leak")
  expect_error(tr_ml_fit(s$teste, "Species", modelo = "linear"), class = "tr_ml_error_test_leak")
  expect_error(tr_ml_tune(s$teste, "Species", tentativas = 2, folds = 2), class = "tr_ml_error_test_leak")
  expect_error(tr_ml_nested_cv(s$teste, "Species", tentativas = 2, folds_externos = 2, folds = 2),
               class = "tr_ml_error_test_leak")
  expect_error(tr_ml_split(s$teste, "Species"), class = "tr_ml_error_test_leak")
  # a marca sobrevive a filtro, subconjunto e coluna nova
  f <- s$teste[s$teste$Sepal.Length > 5, ]; f$novo <- 1
  expect_error(tr_ml_cart(f, "Species"), class = "tr_ml_error_test_leak")
  # treino e dividir o treino de novo (validação) continuam permitidos
  expect_s3_class(tr_ml_cart(s$treino, "Species"), "tr_ml_fit")
  v <- tr_ml_split(s$treino, "Species", seed = 9)
  expect_equal(attr(v$treino, "tr_ml_origem")$papel, "treino")
})

test_that("tune com treino marcado mede os folds sem recusa e o modelo herda a origem", {
  s <- tr_ml_split(binaria(), "Species")
  z <- tr_ml_tune(s$treino, "Species", tentativas = 2, folds = 2)
  expect_true(all(z$historico$status == "ok"))
  expect_identical(z$modelo$origem, attr(s$treino, "tr_ml_origem"))
  n <- tr_ml_nested_cv(s$treino, "Species", tentativas = 2, folds_externos = 2, folds = 2)
  expect_true(all(is.finite(n$externa)))
})

test_that("avaliar o treino: erro por padrão, nota e aviso com permitir_treino", {
  s <- tr_ml_split(binaria(), "Species")
  m <- tr_ml_cart(s$treino, "Species")
  pt <- tr_ml_predict(m, s$treino)
  expect_equal(attr(pt, "tr_ml_origem")$papel, "treino")
  expect_error(tr_ml_evaluate(pt, "Species"), class = "tr_ml_error_train_eval")
  expect_error(tr_ml_confusion(pt, "Species"), class = "tr_ml_error_train_eval")
  expect_error(tr_ml_roc(pt, "Species", ".prob_virginica"), class = "tr_ml_error_train_eval")
  expect_error(tr_ml_pr_curve(pt, "Species", ".prob_virginica"), class = "tr_ml_error_train_eval")
  expect_error(tr_ml_evaluate(pt, "Species", permitir_treino = NA), class = "tr_ml_error_bad_param")

  expect_warning(e <- tr_ml_evaluate(pt, "Species", permitir_treino = TRUE), "otimista")
  sem <- pt; attr(sem, "tr_ml_origem") <- NULL
  ref <- tr_ml_evaluate(sem, "Species")
  expect_equal(e$valor, ref$valor)                     # mesmo número, só anotado
  expect_true(all(grepl("otimista", e$nota)))
  expect_false("nota" %in% names(ref))
  expect_warning(cf <- tr_ml_confusion(pt, "Species", permitir_treino = TRUE), "otimista")
  expect_equal(cf$n, tr_ml_confusion(sem, "Species")$n)
  expect_warning(r <- tr_ml_roc(pt, "Species", ".prob_virginica", permitir_treino = TRUE), "otimista")
  expect_match(r$labels$caption, "otimista")
  expect_equal(r$data$auc, tr_ml_roc(sem, "Species", ".prob_virginica")$data$auc)
  expect_warning(pr <- tr_ml_pr_curve(pt, "Species", ".prob_virginica", permitir_treino = TRUE), "otimista")
  expect_true(all(grepl("otimista", pr$data$nota)))
})

test_that("avaliar o teste e tabelas sem marca seguem como antes", {
  s <- tr_ml_split(binaria(), "Species")
  m <- tr_ml_cart(s$treino, "Species")
  p <- tr_ml_predict(m, s$teste)
  expect_equal(attr(p, "tr_ml_origem")$papel, "teste")
  expect_no_warning(e <- tr_ml_evaluate(p, "Species"))
  expect_false("nota" %in% names(e))
  expect_no_warning(tr_ml_roc(p, "Species", ".prob_virginica"))
  # divisão feita pelo usuário (sem marca): nada muda, nem no treino
  d <- binaria(); idx <- seq(1, 100, 2)
  m2 <- tr_ml_cart(d[idx, ], "Species")
  expect_null(m2$origem)
  expect_no_warning(tr_ml_evaluate(tr_ml_predict(m2, d[idx, ]), "Species"))
  expect_s3_class(tr_ml_cart(d[-idx, ], "Species"), "tr_ml_fit")
})

test_that("prever o teste de outra divisão com o modelo desta é recusado", {
  s1 <- tr_ml_split(binaria(), "Species", seed = 1)
  s2 <- tr_ml_split(binaria(), "Species", seed = 2)
  m <- tr_ml_cart(s1$treino, "Species")
  expect_error(tr_ml_predict(m, s2$teste), class = "tr_ml_error_split_mismatch")
  expect_s3_class(tr_ml_predict(m, s1$teste), "tbl_df")
  # modelo sem origem (divisão por fora) prevê qualquer teste
  sem <- s1$treino; attr(sem, "tr_ml_origem") <- NULL
  expect_s3_class(tr_ml_predict(tr_ml_cart(sem, "Species"), s2$teste), "tbl_df")
})

test_that("marca sobrevive ao store e à releitura do cache no fluxo real", {
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  trama::tr_use("trama.view", registry = reg)
  trama::tr_use(trama_collection(), registry = reg)
  f <- trama::tr_flow(reg) |>
    trama::tr_add("dados", "ml/example", nome = "iris_binaria") |>
    trama::tr_add("divisao", "ml/split", alvo = "Species", from = "dados") |>
    trama::tr_add("errado", "ml/cart", alvo = "Species", from = "divisao:teste") |>
    trama::tr_add("modelo", "ml/cart", alvo = "Species", from = "divisao:treino") |>
    trama::tr_add("no_treino", "ml/predict", from = c("modelo", "divisao:treino")) |>
    trama::tr_add("aval_treino", "ml/evaluate", alvo = "Species", from = "no_treino") |>
    trama::tr_add("aval_treino_ok", "ml/evaluate", alvo = "Species", permitir_treino = TRUE, from = "no_treino") |>
    trama::tr_add("no_teste", "ml/predict", from = c("modelo", "divisao:teste")) |>
    trama::tr_add("aval_teste", "ml/evaluate", alvo = "Species", from = "no_teste")
  store <- trama::tr_store(tempfile())
  doc <- trama::tr_flow_doc(f)
  val <- function(id, port = NULL) trama::tr_value(doc, id, port = port, registry = reg, store = store)
  for (rodada in 1:2) {   # a segunda lê tudo do cache (restore do RDS)
    expect_equal(attr(val("divisao", "teste"), "tr_ml_origem")$papel, "teste", info = rodada)
    expect_error(val("errado"), "treina no teste", info = rodada)
    expect_error(val("aval_treino"), "otimista", info = rodada)
    ok <- suppressWarnings(val("aval_treino_ok"))
    expect_true(all(grepl("otimista", ok$nota)), info = rodada)
    expect_false("nota" %in% names(val("aval_teste")), info = rodada)
    expect_equal(val("modelo")$origem$papel, "treino", info = rodada)
  }
  typ <- trama.data::trama_collection()$types
  tabela <- Filter(function(t) identical(t$id, "data/table"), typ)[[1L]]
  path <- tempfile(fileext = ".rds")
  tabela$store(val("divisao", "teste"), path)
  expect_error(tr_ml_cart(tabela$restore(path), "Species"), class = "tr_ml_error_test_leak")
})
