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
