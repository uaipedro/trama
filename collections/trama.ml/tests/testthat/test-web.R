# `tr_models_web()` dos motores da ml: o `prever` roda num R à parte, sem o
# trama (como no WebR), sobre o motor lido de RDS. Oráculo: a previsão do
# modelo completo pela `tr_models_predict_raw()`; tolerância 1e-12 (o mesmo
# predict do motor; no XGBoost, depois do UBJ, que guarda float32 — e a
# previsão original também vem do booster em float32).

test_that("cada motor prevê fora do trama igual ao modelo completo", {
  skip_if_not_installed("callr")
  for (p in c("rpart", "ranger", "e1071", "xgboost")) skip_if_not_installed(p)
  d <- dados_ml_reduzir()
  casos <- list(); refs <- list(); ws <- list()
  for (mod in c("linear", "cart", "forest", "svm", "xgboost")) for (resp in c("y", "cl", "c3")) {
    if (mod == "linear" && resp == "c3") next
    m <- tr_ml_fit(d, resposta = resp, preditores = "x1, x2, x3", modelo = mod)
    k <- paste(mod, resp)
    ws[[k]] <- trama.models::tr_models_web(m)
    refs[[k]] <- trama.models::tr_models_predict_raw(m, novos_ml)
    casos[[k]] <- list(motor = ws[[k]]$motor, prever = ws[[k]]$prever, novos = novos_ml)
  }
  fora <- callr::r(function(casos) {
    lapply(casos, function(cs) {
      f <- tempfile(fileext = ".rds"); saveRDS(cs$motor, f)
      prever <- eval(parse(text = cs$prever))
      prever(readRDS(f), cs$novos)
    })
  }, args = list(casos = casos))
  for (k in names(casos)) {
    ref <- refs[[k]]
    expect_identical(names(fora[[k]]), ws[[k]]$saida$colunas, label = k)
    if (is.null(ref$prob)) {
      expect_equal(fora[[k]]$previsto, ref$previsto, tolerance = 1e-12, label = k)
    } else {
      expect_identical(fora[[k]]$previsto, as.character(ref$previsto), label = k)
      expect_equal(unname(as.matrix(fora[[k]][, -1])), unname(ref$prob), tolerance = 1e-12, label = k)
    }
  }
  expect_identical(names(ws[["forest y"]]$pacotes), c("stats", "ranger"))
  expect_true(is.raw(ws[["xgboost y"]]$motor))
})

test_that("FIGS não ganha preditor na página", {
  skip_if_not_installed("figsr")
  m <- tr_ml_fit(dados_ml_reduzir(), resposta = "y", preditores = "x1, x2, x3", modelo = "figs")
  expect_null(trama.models::tr_models_web(m))
})
