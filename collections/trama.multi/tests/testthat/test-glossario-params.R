# Glossário de params (docs/glossario-parametros.md): na discriminante e na
# logística `grupo`/`cols` viraram `resposta`/`preditores`, e `nivel` virou
# `confianca`. Fluxo salvo com o nome antigo tem de abrir migrado.

doc_minimo <- function(tipo, params) {
  list(nodes = list(n = list(type = tipo, params = params)), edges = list())
}

test_that("fluxos salvos com grupo/cols/nivel abrem com resposta/preditores/confianca", {
  reg <- multi_registry()
  mig <- function(tipo, params) trama::tr_doc_migrate(doc_minimo(tipo, params), reg)$nodes$n$params
  for (tipo in c("multi/discriminant", "multi/logistic")) {
    p <- mig(tipo, list(grupo = "Species", cols = "Petal.Length"))
    expect_null(p$grupo)
    expect_null(p$cols)
    expect_equal(p$resposta, "Species")
    expect_equal(p$preditores, "Petal.Length")
  }
  p <- mig("multi/jackknife_pca", list(nivel = 0.9))
  expect_null(p$nivel)
  expect_equal(p$confianca, 0.9)
  # O M de Box não é modelo: `grupo` e `cols` ficam como estão.
  p <- mig("multi/box_m", list(grupo = "cultivar", cols = "alcool"))
  expect_equal(p$grupo, "cultivar")
  expect_equal(p$cols, "alcool")
})

test_that("classify, confusion, roc e logistic_coefficients abrem como os blocos da models", {
  # A migração é declarada na `trama.models` (o destino é dela); aqui se confere
  # o documento inteiro de um fluxo da multi, com as arestas do modelo.
  reg <- multi_registry()
  doc <- list(
    nodes = list(
      d = list(type = "multi/example", params = list(dataset = "pima")),
      lda = list(type = "multi/discriminant", params = list(grupo = "diabetes")),
      lg = list(type = "multi/logistic", params = list(grupo = "diabetes")),
      cab = list(type = "data/slice_head", params = list(n = 5L)),
      cl = list(type = "multi/classify", params = list(validacao = "cruzada")),
      cf = list(type = "multi/confusion", params = list()),
      roc = list(type = "multi/roc", params = list(validacao = "resubstituição")),
      rc = list(type = "multi/logistic_coefficients", params = list(nivel = 0.9, escala = "desvio padrão"))),
    edges = list(
      list(from = list(node = "d", port = "out"), to = list(node = "lda", port = "dados")),
      list(from = list(node = "d", port = "out"), to = list(node = "lg", port = "dados")),
      list(from = list(node = "d", port = "out"), to = list(node = "cab", port = "dados")),
      list(from = list(node = "lda", port = "out"), to = list(node = "cl", port = "modelo")),
      list(from = list(node = "cab", port = "out"), to = list(node = "cl", port = "novos")),
      list(from = list(node = "lda", port = "out"), to = list(node = "cf", port = "modelo")),
      list(from = list(node = "lg", port = "out"), to = list(node = "roc", port = "modelo")),
      list(from = list(node = "lg", port = "out"), to = list(node = "rc", port = "modelo"))))
  m <- trama::tr_doc_migrate(doc, reg)
  tipos <- vapply(m$nodes, `[[`, "", "type")
  expect_equal(unname(tipos[c("cl", "cf", "roc", "rc")]),
               c("models/predict", "models/confusion", "models/roc", "models/coefficients"))
  expect_equal(m$nodes$cl$params$validacao, "cruzada")
  expect_equal(m$nodes$roc$params$validacao, "resubstituição")
  expect_equal(m$nodes$rc$params, list(escala = "desvio padrão", exponenciar = TRUE, confianca = 0.9))
  expect_equal(m$edges[[5]]$to, list(node = "cl", port = "dados"))
  expect_length(trama::tr_doc_validate(m, reg), 0L)
  s <- trama::tr_store(tempfile())
  rc <- trama::tr_value(m, "rc", registry = reg, store = s)
  expect_true(all(c("li_90", "ls_90") %in% names(rc$tabela)))
  expect_equal(nrow(trama::tr_value(m, "cl", registry = reg, store = s)), 5L)
})
