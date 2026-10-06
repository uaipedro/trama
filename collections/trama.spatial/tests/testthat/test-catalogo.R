test_that("todo nó tem description, categoria existente e portas de tipo conhecido", {
  co <- trama_collection()
  cats <- vapply(co$categories, function(x) x$id, "")
  tipos <- c(vapply(co$types, function(x) x$id, ""), "data/table", "view/plot")
  for (nd in co$nodes) {
    expect_true(nzchar(nd$description), info = nd$id)
    expect_true(nd$category %in% cats, info = nd$id)
    for (p in c(nd$inputs, nd$outputs)) {
      tp <- if (inherits(p, "tr_port")) p$type else p
      expect_true(tp %in% tipos, info = paste(nd$id, tp))
    }
  }
})

test_that("todo nó tem ajuda não vazia", {
  for (nd in trama_collection()$nodes) {
    expect_true(!is.null(nd$help) && nzchar(nd$help), info = nd$id)
  }
})

test_that("nenhum nó usa um nome de param que o glossário reserva", {
  # `rotulo` sozinho é a COLUNA de rótulos (docs/glossario-parametros.md:34);
  # texto livre de nomeação é `nome`. `coluna` é proibido em favor de `variavel`.
  proibidos <- c("coluna", "alfa", "nivel", "alvo", "rotulo")
  for (nd in trama_collection()$nodes) {
    ps <- vapply(nd$params, function(p) p$name %||% "", "")
    expect_length(intersect(ps, proibidos), 0L)
  }
})

test_that("o catálogo sai inteiro com data e view carregadas", {
  reg <- spatial_registry()
  cat <- trama::tr_catalog(registry = reg)
  ids <- vapply(cat$nodes, function(x) x$id, "")
  expect_true(all(c("spatial/example", "spatial/coordinates") %in% ids))
})
