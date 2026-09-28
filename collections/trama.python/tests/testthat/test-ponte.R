# Oráculo: o próprio scikit-learn chamado direto, com a mesma semente. A ponte
# não pode mudar um número — só traduzir tipos e reordenar colunas por nome.

test_that("classificação: probabilidades iguais ao sklearn direto, colunas pelos níveis do R", {
  skip_sem_python()
  d <- iris[c(51:150, 1:50), ]  # ordem de chegada != ordem alfabética
  m <- tr_py_forest(d, "Species", trees = 50L, seed = 7L)
  p <- trama.models::tr_models_predict_raw(m, iris)
  X <- reticulate::r_to_py(iris[, 1:4])
  ref <- sk()$RandomForestClassifier(n_estimators = 50L, random_state = 7L)$fit(
    reticulate::r_to_py(d[, 1:4]), as.character(d$Species))
  pr <- ref$predict_proba(X); colnames(pr) <- as.character(ref$classes_)
  expect_equal(colnames(p$prob), levels(iris$Species))
  expect_equal(unname(p$prob), unname(pr[, levels(iris$Species)]), tolerance = 0)
  expect_equal(levels(p$previsto), levels(iris$Species))
})

test_that("regressão e cruzada batem com cross_val_predict", {
  skip_sem_python()
  m <- tr_py_forest(mtcars, "mpg", preditores = "wt, hp, disp", trees = 30L, seed = 3L)
  cv <- trama.models::tr_models_predict_cv(m, "cruzada")
  ms <- reticulate::import("sklearn.model_selection")
  ref <- ms$cross_val_predict(
    sk()$RandomForestRegressor(n_estimators = 30L, random_state = 3L),
    reticulate::r_to_py(mtcars[, c("wt", "hp", "disp")]), mtcars$mpg,
    cv = ms$KFold(n_splits = 5L, shuffle = TRUE, random_state = 3L))
  expect_equal(cv$previsto, as.numeric(ref), tolerance = 0)
  expect_null(cv$prob)
})

test_that("store: o modelo vira pickle, volta, e prevê igual (processo sem objeto vivo)", {
  skip_sem_python()
  r <- py_registry()
  m <- tr_py_forest(iris, "Species", trees = 20L, seed = 1L)
  ty <- trama::tr_get_type("models/fit", r)
  s <- trama::tr_store(file.path(tempfile(), "s"))
  h <- trama::tr_store_put(s, "k", m, ty)
  volta <- trama::tr_store_get(s, "k", ty)
  expect_true(is.raw(volta$estimador))
  expect_equal(trama.models::tr_models_predict_raw(volta, iris)$prob,
               trama.models::tr_models_predict_raw(m, iris)$prob)
})

test_that("fluxo: data/example -> python/forest -> models/evaluate, sequencial e no pool mirai", {
  skip_sem_python(); skip_if_not_installed("mirai")
  r <- py_registry()
  doc <- trama::tr_flow(r) |>
    trama::tr_add("dados", "data/example", dataset = "iris") |>
    trama::tr_add("rf", "python/forest", from = "dados", resposta = "Species", trees = 30L) |>
    trama::tr_add("aval", "models/evaluate", from = "rf", validacao = "cruzada")
  doc <- doc$doc
  ty <- trama::tr_get_type("data/table", r)
  s1 <- trama::tr_store(file.path(tempfile(), "s"))
  seq <- trama::tr_run(doc, registry = r, store = s1)
  expect_setequal(seq$done, c("dados", "rf", "aval"))
  a1 <- trama::tr_store_get(s1, seq$results$aval$out$key, ty)
  expect_gt(a1$valor[a1$metrica == "accuracy"], 0.9)

  raiz <- normalizePath(test_path("../../../.."))
  setup <- bquote({
    pkgload::load_all(.(raiz), quiet = TRUE, attach = FALSE, export_all = FALSE)
    for (p in c("trama.data", "trama.view", "trama.models", "trama.python"))
      pkgload::load_all(file.path(.(raiz), "collections", p), quiet = TRUE, attach = FALSE, export_all = FALSE)
  })
  ex <- trama::tr_executor_pool(2L, registry = r, setup = setup)
  on.exit(ex$shutdown(), add = TRUE)
  s2 <- trama::tr_store(file.path(tempfile(), "s"))
  par <- trama::tr_run(doc, registry = r, store = s2, executor = ex)
  expect_setequal(par$done, c("dados", "rf", "aval"))
  expect_equal(trama::tr_store_get(s2, par$results$aval$out$key, ty), a1)

  # Segunda rodada no mesmo store: tudo do cache (a impressão digital é estável).
  ev <- character()
  trama::tr_run(doc, registry = r, store = s1, on_event = function(e) ev[[length(ev) + 1L]] <<- e$type)
  expect_false("running" %in% ev)
  expect_true("cached" %in% ev)
})
