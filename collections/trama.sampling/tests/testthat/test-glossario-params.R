# Glossário de params (docs/glossario-parametros.md): `coluna` do size_mean
# virou `variavel`. O fluxo salvo com o nome antigo tem de abrir migrado.

test_that("size_mean salvo com coluna abre com variavel", {
  doc <- list(nodes = list(n = list(type = "sampling/size_mean", params = list(coluna = "producao_t"))),
              edges = list())
  p <- trama::tr_doc_migrate(doc, sampling_registry())$nodes$n$params
  expect_null(p$coluna)
  expect_equal(p$variavel, "producao_t")
})
