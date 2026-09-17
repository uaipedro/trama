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

test_that("formal de step que não é input nem param é recusado, e .seed é aceito", {
  base <- function(step) {
    tr_node("x/g", fn = function() NULL, description = "d",
            inputs = list(x = tr_port("data/table", stream = TRUE)),
            init = function() list(), step = step)
  }
  expect_error(base(function(state, x, zzz) list(state = state, out = x)),
               class = "tr_error_bad_step")
  # `.seed` é permitido em `step` pelo mesmo motivo que em `fn`. Este teste
  # existe porque tirar `.seed` da lista de permitidos passaria a recusar nó
  # VÁLIDO, e nenhum outro teste quebraria.
  expect_true(base(function(state, x, .seed) list(state = state, out = x))$online)
})

test_that("online aparece no catálogo, e o campo some no nó comum", {
  com_memoria <- tr_node("t/online", fn = function() NULL, description = "Acumula ponto a ponto.",
                         inputs = list(x = tr_port("t/num", stream = TRUE)),
                         init = function() list(soma = 0),
                         step = function(state, x) list(state = state, out = x))
  comum <- tr_node("t/comum", fn = function(x) x, description = "Passa adiante.",
                   inputs = list(x = "t/num"), outputs = list(out = "t/num"))
  col <- tr_collection(id = "t", types = list(tr_type("t/num")),
                       nodes = list(com_memoria, comum))
  reg <- tr_registry(); tr_use(col, registry = reg)
  cat_nodes <- tr_catalog(reg)$nodes
  by_id <- function(id) Filter(function(n) identical(n$id, id), cat_nodes)[[1]]

  expect_true(by_id("t/online")$online)
  expect_false("online" %in% names(by_id("t/comum")))
})

test_that("nó com memória sem saída de fluxo é recusado na declaração", {
  # A armadilha que custou uma rodada de depuração ao autor do primeiro
  # `models/rls`: "declara entrada de fluxo, não declara saída de fluxo" é
  # EXATAMENTE o predicado de colapso de `.tr_stream_regions()`, então o motor
  # lia o nó com memória como o fim da região. Ela parava um nó antes, o
  # colapso de verdade nunca entrava nela, e nada errava alto.
  memoria <- function(saida) {
    tr_node("x/mem", fn = function() NULL, description = "d",
            inputs = list(x = tr_port("data/table", stream = TRUE)),
            outputs = saida,
            init = function() list(n = 0),
            step = function(state, x) list(state = state, out = x))
  }

  err <- expect_error(memoria(list(out = "data/table")),
                      class = "tr_error_online_without_stream_output")
  expect_match(conditionMessage(err), "stream = TRUE")

  # Com a saída declarada como fluxo, nasce normal.
  expect_true(memoria(list(out = tr_port("data/table", stream = TRUE)))$online)
  # E sem porta de saída nenhuma também: terminal que só acumula e publica
  # parcial é forma legítima; o que se recusa é ter saída e nenhuma ser fluxo.
  expect_true(memoria(list())$online)
})

test_that("nó com memória nunca é classificado como colapso", {
  # Defesa em profundidade sobre a validação acima: mesmo que alguém construa a
  # spec à mão, a detecção distingue por CONSTRUÇÃO — colapso roda `fn` uma vez
  # depois do laço, nó com memória roda `step` a cada passo.
  reg <- stream_registry()
  spec <- tr_get_node("s/acumula", reg)
  expect_true(spec$online)
  # `s/acumula` declara saída de fluxo, então nem cai no predicado; o ponto é
  # que `online` sozinho já o exclui.
  expect_true(any(vapply(spec$outputs, function(p) isTRUE(p$stream), logical(1))))
})
