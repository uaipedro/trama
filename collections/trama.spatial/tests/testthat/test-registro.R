test_that("a coleção registra com as cinco categorias e o id certo", {
  co <- trama_collection()
  expect_equal(co$id, "spatial")
  expect_equal(co$version, "0.2.0")
  ids <- vapply(co$categories, function(x) x$id, "")
  expect_equal(ids, c("espacial_fonte", "espacial_preparar", "espacial_explorar",
                      "espacial_variograma", "espacial_predizer"))
  # Prefixo obrigatório: o registro de categorias é global.
  expect_true(all(startsWith(ids, "espacial_")))
})

test_that("a coleção entra num registro com data e view na frente", {
  reg <- spatial_registry()
  expect_true("spatial" %in% names(reg$collections))
})
