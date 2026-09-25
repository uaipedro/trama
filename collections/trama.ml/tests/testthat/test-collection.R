# O tipo `models/fit` é da trama.models: store/restore são os de lá, que
# chamam `tr_models_serialize()`/`unserialize()` da classe.
fit_type <- function() {
  Filter(function(t) identical(t$id, "models/fit"), trama.models::trama_collection()$types)[[1L]]
}

test_that("catálogo registra os 15 blocos e contratos consistentes", {
  reg <- ml_registry()
  nodes <- trama_collection()$nodes
  expect_length(nodes, 15L)  # + ml/nested_cv e ml/pr_curve (main)
  ids <- vapply(nodes, `[[`, "", "id")
  # Prever, avaliar, confusão, ROC e importância são os blocos da models.
  expect_false(any(c("ml/predict", "ml/evaluate", "ml/confusion", "ml/roc", "ml/importance") %in% ids))
  expect_length(trama_collection()$types, 0L)
  for (n in nodes) {
    for (sec in c("Descrição", "Parâmetros", "Valor", "Exemplos", "Veja também"))
      expect_match(n$help, paste0("## ", sec), fixed = TRUE, info = n$id)
    for (p in names(n$params)) {
      expect_equal(n$params[[p]]$default, eval(formals(n$fn)[[p]]), info = paste(n$id, p))
    }
    for (p in Filter(function(p) p$kind %in% c("text", "cols"), n$params))
      expect_true(nzchar(p$example), info = n$id)
    expect_false(grepl("ml/fit", n$help, fixed = TRUE), info = n$id)
  }
  por_id <- stats::setNames(nodes, ids)
  for (id in c("ml/linear", "ml/cart", "ml/figs", "ml/forest", "ml/svm", "ml/xgboost"))
    expect_equal(por_id[[id]]$outputs$out$type, "models/fit", info = id)
  expect_equal(por_id[["ml/tune"]]$outputs$modelo$type, "models/fit")
  for (id in c("ml/rules", "ml/tree_plot"))
    expect_equal(por_id[[id]]$inputs$modelo$type, "models/fit", info = id)
  expect_match(as.character(trama::tr_catalog_json(reg)), "ml/figs", fixed = TRUE)
})

test_that("fluxo real calcula teste separado com os blocos da models", {
  reg <- ml_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("dados", "ml/example", nome = "iris_binaria") |>
    trama::tr_add("divisao", "ml/split", resposta = "Species", from = "dados") |>
    trama::tr_add("modelo", "ml/cart", resposta = "Species", from = "divisao:treino") |>
    trama::tr_add("prever", "models/predict", from = c("modelo", "divisao:teste")) |>
    trama::tr_add("avaliar", "models/evaluate", resposta = "Species", from = "prever") |>
    trama::tr_add("direto", "models/evaluate", from = c("modelo", "divisao:teste")) |>
    trama::tr_add("imp", "models/importance", from = "modelo")
  store <- trama::tr_store(tempfile())
  doc <- trama::tr_flow_doc(f)
  val <- function(id, port = NULL) trama::tr_value(doc, id, port = port, registry = reg, store = store)
  m <- val("modelo")
  p <- val("prever")
  expect_s3_class(m, "tr_models_fit")
  expect_equal(m$n, 74L)
  expect_equal(nrow(p), 26L)
  expect_true(all(c("previsto", "prob_versicolor", "prob_virginica") %in% names(p)))
  av <- val("avaliar")
  expect_equal(unique(av$n[is.na(av$classe)]), 26L)  # por classe, `n` é o suporte
  # Modo tabela e modo modelo+dados medem o mesmo teste.
  expect_equal(av$valor, val("direto")$valor)
  expect_equal(p, prever(m, val("divisao", "teste")))
  expect_true(all(val("imp")$termo %in% m$preditores))
})

test_that("fluxo real ajusta tuning e expõe modelo e histórico", {
  reg <- ml_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("dados", "ml/example", nome = "mtcars") |>
    trama::tr_add("ajuste", "ml/tune", resposta = "mpg", preditores = "wt, hp",
                  tentativas = 2L, folds = 2L, from = "dados")
  store <- trama::tr_store(tempfile())
  doc <- trama::tr_flow_doc(f)
  modelo <- trama::tr_value(doc, "ajuste", port = "modelo", registry = reg, store = store)
  historico <- trama::tr_value(doc, "ajuste", port = "historico", registry = reg, store = store)
  expect_s3_class(modelo, "tr_ml_fit")
  expect_s3_class(modelo, "tr_models_fit")
  expect_equal(nrow(historico), 2L)
})

test_that("todos os motores persistem como models/fit com previsões iguais", {
  engines <- c(cart = "rpart", figs = "figsr", forest = "ranger", svm = "e1071", xgboost = "xgboost", linear = "stats")
  typ <- fit_type()
  d <- tr_ml_example("iris_binaria")
  for (engine in names(engines)) {
    if (!requireNamespace(engines[[engine]], quietly = TRUE)) next
    m <- tr_ml_fit(d, "Species", modelo = engine, trees = 10L, nrounds = 3L)
    path <- tempfile(fileext = ".rds")
    typ$store(m, path)
    if (engine == "xgboost") expect_true(is.raw(readRDS(path)$ajuste))
    restored <- typ$restore(path)
    expect_equal(prever(restored, d[1:3, ]), prever(m, d[1:3, ]), info = engine)
    pv <- typ$preview(restored, NULL)
    expect_equal(pv$renderer, "data/table")
    expect_gt(pv$data$nrow, 0)
  }
})

test_that("xgboost e floresta voltam do store real de um fluxo, em registro novo", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("ranger")
  root <- tempfile()
  doc <- trama::tr_flow_doc(trama::tr_flow(ml_registry()) |>
    trama::tr_add("dados", "ml/example", nome = "iris") |>
    trama::tr_add("xgb", "ml/xgboost", resposta = "Species", nrounds = 5L, from = "dados") |>
    trama::tr_add("rf", "ml/forest", resposta = "Species", trees = 20L, from = "dados"))
  trama::tr_run(doc, registry = ml_registry(), store = trama::tr_store(root))
  # Registro e store novos sobre a mesma pasta: o valor sai do disco (RDS +
  # booster em UBJ), e não da memória da sessão que ajustou.
  reg2 <- ml_registry()
  store2 <- trama::tr_store(root)
  novos <- iris[c(1, 51, 101), ]
  for (id in c("xgb", "rf")) {
    m <- trama::tr_value(doc, id, registry = reg2, store = store2)
    expect_s3_class(m, "tr_ml_fit")
    p <- prever(m, novos)
    expect_equal(as.character(p$previsto), c("setosa", "versicolor", "virginica"), info = id)
  }
  xgb <- trama::tr_value(doc, "xgb", registry = reg2, store = store2)
  expect_s3_class(xgb$ajuste, "xgb.Booster")
})

test_that("leitores da ml recusam modelo de outra coleção com classe", {
  lm_models <- trama.models::tr_models_lm(mtcars, formula = "mpg ~ wt")
  expect_error(tr_ml_rules(lm_models), class = "tr_ml_error_not_fit")
  expect_error(tr_ml_rules(lm_models), "tr_models_lm")
  expect_error(tr_ml_tree_plot(lm_models), class = "tr_ml_error_not_fit")
})

test_that("exemplos da ajuda executam", {
  engines <- c(figs = "figsr", forest = "ranger", svm = "e1071", xgboost = "xgboost", cart = "rpart")
  for (n in trama_collection()$nodes) {
    id <- sub("ml/", "", n$id, fixed = TRUE)
    if (id %in% names(engines) && !requireNamespace(engines[[id]], quietly = TRUE)) next
    code <- regmatches(n$help, regexpr("(?s)```r\n.*?\n```", n$help, perl = TRUE))
    code <- sub("^```r\n", "", sub("\n```$", "", code))
    expect_no_error(eval(parse(text = code), envir = new.env()), message = n$id)
  }
})

# Glossário de params (docs/glossario-parametros.md): `alvo` virou `resposta` e,
# nos ajustes, `cols` virou `preditores`. Fluxo salvo com o nome antigo abre migrado.
test_that("fluxos salvos com alvo/cols abrem com resposta/preditores", {
  reg <- ml_registry()
  mig <- function(tipo, params) {
    doc <- list(nodes = list(n = list(type = tipo, params = params)), edges = list())
    trama::tr_doc_migrate(doc, reg)$nodes$n
  }
  p <- mig("ml/cart", list(alvo = "Species", cols = "Petal.Length"))$params
  expect_null(p$alvo)
  expect_null(p$cols)
  expect_equal(p$resposta, "Species")
  expect_equal(p$preditores, "Petal.Length")
  expect_equal(mig("ml/residuals", list(alvo = "mpg", predito = ".pred"))$params,
               list(resposta = "mpg", predito = "previsto"))
})

# Os leitores que foram para a models (Fase 4): a migração é declarada LÁ, e o
# fluxo antigo abre com o id novo, os params renomeados e as colunas `.pred` /
# `.prob_` convertidas para `previsto` / `prob_`.
test_that("leitores antigos da ml abrem como os blocos da models", {
  reg <- ml_registry()
  mig <- function(tipo, params = list()) {
    doc <- list(nodes = list(n = list(type = tipo, params = params)), edges = list())
    trama::tr_doc_migrate(doc, reg)$nodes$n
  }
  n <- mig("ml/evaluate", list(alvo = "mpg", predito = ".pred", tarefa = "regressao"))
  expect_equal(n$type, "models/evaluate")
  expect_equal(n$params, list(resposta = "mpg", predito = "previsto"))
  n <- mig("ml/confusion", list(alvo = "y", predito = ".pred"))
  expect_equal(n$type, "models/confusion")
  expect_equal(n$params, list(resposta = "y", predito = "previsto"))
  n <- mig("ml/roc", list(alvo = "y", probabilidade = ".prob_sim", positiva = "sim"))
  expect_equal(n$type, "models/roc")
  expect_equal(n$params[c("resposta", "probabilidade", "positiva")],
               list(resposta = "y", probabilidade = "prob_sim", positiva = "sim"))
  expect_equal(mig("ml/predict")$type, "models/predict")
  expect_equal(mig("ml/importance")$type, "models/importance")
  # Reabrir não mexe: `when` só reconhece o formato velho.
  expect_equal(mig("models/evaluate", list(predito = "previsto"))$params, list(predito = "previsto"))
  expect_equal(mig("models/roc", list(probabilidade = "prob_sim"))$params, list(probabilidade = "prob_sim"))
})
