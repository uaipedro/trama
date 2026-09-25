ml_registry <- function() {
  r <- trama::tr_registry()
  trama::tr_use("trama.data", registry = r)
  trama::tr_use("trama.view", registry = r)
  trama::tr_use(trama_collection(), registry = r)
  r
}

test_that("catálogo registra os 19 blocos e contratos consistentes", {
  reg <- ml_registry()
  nodes <- trama_collection()$nodes
  expect_length(nodes, 19L)
  for (n in nodes) {
    for (sec in c("Descrição", "Parâmetros", "Valor", "Exemplos", "Veja também"))
      expect_match(n$help, paste0("## ", sec), fixed = TRUE, info = n$id)
    for (p in names(n$params)) {
      expect_equal(n$params[[p]]$default, eval(formals(n$fn)[[p]]), info = paste(n$id, p))
    }
    for (p in Filter(function(p) p$kind %in% c("text", "cols"), n$params))
      expect_true(nzchar(p$example), info = n$id)
  }
  expect_match(as.character(trama::tr_catalog_json(reg)), "ml/figs", fixed = TRUE)
})

test_that("fluxo real calcula teste separado, preserva e restaura modelos", {
  reg <- ml_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("dados", "ml/example", nome = "iris_binaria") |>
    trama::tr_add("divisao", "ml/split", alvo = "Species", from = "dados") |>
    trama::tr_add("modelo", "ml/cart", alvo = "Species", from = "divisao:treino") |>
    trama::tr_add("prever", "ml/predict", from = c("modelo", "divisao:teste")) |>
    trama::tr_add("avaliar", "ml/evaluate", alvo = "Species", from = "prever")
  store <- trama::tr_store(tempfile())
  doc <- trama::tr_flow_doc(f)
  val <- function(id, port = NULL) trama::tr_value(doc, id, port = port, registry = reg, store = store)
  m <- val("modelo")
  p <- val("prever")
  expect_equal(m$n, 74L)
  expect_equal(nrow(p), 26L)
  expect_equal(val("avaliar")$n, rep(26L, 3L))
  expect_equal(val("prever"), p)
  expect_equal(tr_ml_predict(m, val("divisao", "teste")), p)
})

test_that("fluxo real ajusta tuning e expõe modelo e histórico", {
  reg <- ml_registry()
  f <- trama::tr_flow(reg) |>
    trama::tr_add("dados", "ml/example", nome = "mtcars") |>
    trama::tr_add("ajuste", "ml/tune", alvo = "mpg", cols = "wt, hp",
                  tentativas = 2L, folds = 2L, from = "dados")
  store <- trama::tr_store(tempfile())
  doc <- trama::tr_flow_doc(f)
  modelo <- trama::tr_value(doc, "ajuste", port = "modelo", registry = reg, store = store)
  historico <- trama::tr_value(doc, "ajuste", port = "historico", registry = reg, store = store)
  expect_s3_class(modelo, "tr_ml_fit")
  expect_equal(nrow(historico), 2L)
})

test_that("todos os motores persistem com previsões iguais", {
  engines <- c(cart = "rpart", figs = "figsr", forest = "ranger", svm = "e1071", xgboost = "xgboost", linear = "stats")
  typ <- .tr_ml_model_type()
  d <- tr_ml_example("iris_binaria")
  for (engine in names(engines)) {
    if (!requireNamespace(engines[[engine]], quietly = TRUE)) next
    m <- tr_ml_fit(d, "Species", modelo = engine, trees = 10L, nrounds = 3L)
    path <- tempfile(fileext = ".rds")
    typ$store(m, path)
    restored <- typ$restore(path)
    expect_equal(tr_ml_predict(restored, d[1:3, ]), tr_ml_predict(m, d[1:3, ]), info = engine)
    pv <- typ$preview(restored, NULL)
    expect_equal(pv$renderer, "data/table")
    expect_gt(pv$data$nrow, 0)
  }
  expect_error(typ$store(list(), tempfile()), class = "tr_ml_error_model")
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
