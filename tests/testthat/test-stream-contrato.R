test_that("tr_port aceita stream e o catálogo transporta", {
  p <- tr_port("data/table", stream = TRUE)
  expect_true(p$stream)
  expect_false(tr_port("data/table")$stream)
})

test_that("porta de fluxo aparece no catálogo, e o campo some quando não é fluxo", {
  col <- tr_collection(
    id = "t", types = list(tr_type("t/num")),
    nodes = list(tr_node("t/online", fn = function(x, k) x + k,
                         description = "Consome um ponto por vez e devolve o acumulado.",
                         inputs = list(x = tr_port("t/num", stream = TRUE),
                                       k = tr_port("t/num")),
                         outputs = list(out = "t/num")))
  )
  reg <- tr_registry(); tr_use(col, registry = reg)
  n <- tr_catalog(reg)$nodes[[1]]

  expect_true(n$inputs[[1]]$stream)
  # Ausência tem que ser UMA coisa no JSON: a porta comum não carrega o campo,
  # nem como FALSE nem como null.
  expect_false("stream" %in% names(n$inputs[[2]]))
})
