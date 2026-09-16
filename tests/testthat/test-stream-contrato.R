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

test_that("init e step andam juntos", {
  expect_error(tr_node("x/a", fn = function() NULL, description = "d",
                       step = function(state, x) list(state = state, out = x)),
               class = "tr_error_incomplete_online")
})

test_that("nó com estado exige entrada de fluxo", {
  expect_error(tr_node("x/b", fn = function(x) x, description = "d",
                       inputs = list(x = "data/table"),
                       init = function() list(),
                       step = function(state, x) list(state = state, out = x)),
               class = "tr_error_online_without_stream")
})

test_that("step recebe state como primeiro formal", {
  expect_error(tr_node("x/c", fn = function(x) x, description = "d",
                       inputs = list(x = tr_port("data/table", stream = TRUE)),
                       init = function() list(),
                       step = function(x, state) list(state = state, out = x)),
               class = "tr_error_bad_step")
})

test_that("init só recebe nomes de param", {
  expect_error(tr_node("x/d", fn = function(x) x, description = "d",
                       inputs = list(x = tr_port("data/table", stream = TRUE)),
                       init = function(inexistente) list(),
                       step = function(state, x) list(state = state, out = x)),
               class = "tr_error_bad_init")
})

test_that("nó com estado válido nasce com online = TRUE", {
  n <- tr_node("x/e", fn = function(x) x, description = "d",
               inputs = list(x = tr_port("data/table", stream = TRUE)),
               params = list(k = tr_param_num(1)),
               init = function(k) list(soma = 0, n = 0, k = k),
               step = function(state, x) list(state = state, out = x))
  expect_true(n$online)
  expect_false(tr_node("x/f", fn = function() NULL, description = "d")$online)
})
