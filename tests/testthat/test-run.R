events_of <- function(...) { ev <- list(); f <- function(e) ev[[length(ev)+1]] <<- e; list(sink = f, get = function() ev) }
# `run_finished` é ruído em quase todo teste — filtrado por padrão.
types_of  <- function(ev) {
  t <- vapply(ev, function(e) e$type, "")
  t[t != "run_finished"]
}

test_that("roda a cadeia e o valor bate com a chamada direta (nível 1 == grafo)", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "c", params = list(v = 10)),
    list(op = "add_node", type = "t/inc",   id = "i", params = list(by = 5)),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "i", to_port = "x")))

  expect_equal(tr_value(doc, "i", reg, s)$v, 15)
  # Nível 1: a função é a função.
  expect_equal(tr_fn("t/inc", reg)(tr_fn("t/const", reg)(10), 5)$v, 15)
})

test_that("cache: rodar de novo não reexecuta nada", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "c"),
    list(op = "add_node", type = "t/inc",   id = "i"),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "i", to_port = "x")))

  e1 <- events_of(); tr_run(doc, registry = reg, store = s, on_event = e1$sink)
  expect_setequal(types_of(e1$get()), c("running", "done", "running", "done"))

  e2 <- events_of(); tr_run(doc, registry = reg, store = s, on_event = e2$sink)
  expect_equal(types_of(e2$get()), c("cached", "cached"))
})

test_that("editar um param recomputa só o nó e o que depende dele", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "c", params = list(v = 1)),
    list(op = "add_node", type = "t/inc",   id = "i", params = list(by = 1)),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "i", to_port = "x")))
  tr_run(doc, registry = reg, store = s)

  doc2 <- tr_doc_apply(doc, list(op = "set_param", node = "i", name = "by", value = 9), reg)
  ev <- events_of(); tr_run(doc2, registry = reg, store = s, on_event = ev$sink)
  t <- types_of(ev$get())
  expect_equal(t[[1]], "cached")   # `c` intocado
  expect_true("running" %in% t)    # `i` recomputa
  expect_equal(tr_value(doc2, "i", reg, s)$v, 10)
})

test_that("erro vira valor no store, e o jusante bloqueia em vez de reexecutar", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "c"),
    list(op = "add_node", type = "t/boom",  id = "b"),
    list(op = "add_node", type = "t/inc",   id = "i"),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "b", to_port = "x"),
    list(op = "connect", from_node = "b", from_port = "out", to_node = "i", to_port = "x")))

  ev <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev$sink)
  t <- types_of(ev$get())
  expect_true("failed" %in% t)
  failed <- Filter(function(e) e$type == "failed", ev$get())[[1]]
  expect_match(failed$message, "explodiu")
  expect_equal(failed$node, "b")
  expect_false("done" %in% types_of(Filter(function(e) e$node == "i", ev$get())))

  # Segunda passada: `b` não roda de novo, e `i` sai como bloqueado.
  ev2 <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev2$sink)
  t2 <- vapply(ev2$get(), function(e) paste0(e$node, ":", e$type), "")
  expect_true("c:cached" %in% t2)
  expect_true("b:invalid" %in% t2 || "b:blocked" %in% t2 || "b:failed" %in% t2)
  expect_true("i:blocked" %in% t2)
  expect_false(any(grepl("running", t2)))
})

test_that("erro é gravado sob TODAS as portas de saída", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(tr_node("t/split2", fn = function() stop("nao deu"),
                         description = "Falha de propósito, mesmo declarando duas saídas.",
                         outputs = list(a = "t/box", b = "t/box")))), registry = reg)
  s <- tmp_store()
  doc <- build(reg, list(list(op = "add_node", type = "t/split2", id = "s")))
  tr_run(doc, registry = reg, store = s)
  u <- tr_plan(doc, registry = reg, store = s)$units$s
  expect_true(tr_handle_failed(tr_store_handle(s, u$outputs$a)))
  expect_true(tr_handle_failed(tr_store_handle(s, u$outputs$b)))
})

test_that("nó de múltiplas saídas roteia por porta", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(
      tr_node("t/split", fn = function() list(a = list(v = 1), b = list(v = 2)),
              description = "Devolve uma caixa em cada uma das duas saídas.",
              outputs = list(a = "t/box", b = "t/box")),
      tr_node("t/take", fn = function(x) x, inputs = list(x = "t/box"),
              description = "Repassa a caixa que recebe.",
              outputs = list(out = "t/box")))), registry = reg)
  s <- tmp_store()
  mk <- function(port) build(reg, list(
    list(op = "add_node", type = "t/split", id = "sp"),
    list(op = "add_node", type = "t/take",  id = "k"),
    list(op = "connect", from_node = "sp", from_port = port, to_node = "k", to_port = "x")))
  expect_equal(tr_value(mk("a"), "k", reg, s)$v, 1)
  expect_equal(tr_value(mk("b"), "k", reg, s)$v, 2)
})

test_that("saída faltando é erro alto e cedo, não chave ausente lá na frente", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(tr_node("t/mau", fn = function() list(a = 1),
                         description = "Declara duas saídas mas só devolve uma.",
                         outputs = list(a = "t/box", b = "t/box")))), registry = reg)
  s <- tmp_store()
  doc <- build(reg, list(list(op = "add_node", type = "t/mau", id = "m")))
  ev <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev$sink)
  failed <- Filter(function(e) e$type == "failed", ev$get())[[1]]
  expect_equal(failed$class, "tr_error_bad_output")
  expect_match(failed$message, "declara as saídas")
})

test_that("porta variádica chega na ordem do index", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "a", params = list(v = 1)),
    list(op = "add_node", type = "t/const", id = "b", params = list(v = 20)),
    list(op = "add_node", type = "t/sum",   id = "s"),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "s", to_port = "xs"),
    list(op = "connect", from_node = "b", from_port = "out", to_node = "s", to_port = "xs")))
  expect_equal(tr_value(doc, "s", reg, s)$v, 21)
})

test_that("adaptador roda no worker e não aparece como nó", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t",
    types = list(tr_type("t/a"), tr_type("t/b")),
    nodes = list(tr_node("t/src", fn = function() 5, description = "Devolve o número cinco.",
                         outputs = list(out = "t/a")),
                 tr_node("t/dst", fn = function(x) x * 10, inputs = list(x = "t/b"),
                         description = "Multiplica a entrada por dez.",
                         outputs = list(out = "t/b"))),
    adapters = list(tr_adapter("t/a", "t/b", function(v) v + 1))), registry = reg)
  s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/src", id = "s"),
    list(op = "add_node", type = "t/dst", id = "d"),
    list(op = "connect", from_node = "s", from_port = "out", to_node = "d", to_port = "x")))
  expect_equal(tr_value(doc, "d", reg, s), 60)         # (5+1)*10
  expect_setequal(names(tr_plan(doc, registry = reg)$units), c("s", "d"))  # 2 nós, não 3
})

test_that("porta obrigatória solta não é despachada — falha na edição, não no fn", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(list(op = "add_node", type = "t/inc", id = "i")))
  ev <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev$sink)
  expect_equal(types_of(ev$get()), "invalid")
  expect_equal(ev$get()[[1]]$reason, "missing_required_input:x")
})

test_that(".ctx publica progresso pelo store", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(tr_node("t/lento", fn = function(.ctx) {
      for (i in 1:3) .ctx$progress(i / 3, msg = paste("passo", i))
      list(v = 1)
    }, description = "Publica progresso em três passos e devolve um.",
       outputs = list(out = "t/box")))), registry = reg)
  s <- tmp_store()
  doc <- build(reg, list(list(op = "add_node", type = "t/lento", id = "n")))
  unit <- tr_plan(doc, registry = reg, store = s)$units$n
  # Teste do MECANISMO (.ctx$progress escreve, tr_progress lê), não de
  # `tr_run()` de ponta a ponta: desde a Fase 1 o scheduler LIMPA o arquivo
  # de progresso assim que a unidade termina (`.tr_clear_progress`), pra não
  # deixar lixo no store — então ler `tr_progress()` depois de um `tr_run()`
  # completo não encontraria mais nada, por design.
  ctx <- .tr_make_ctx(unit, s)
  for (i in 1:3) ctx$progress(i / 3, msg = paste("passo", i))
  p <- tr_progress(s, unit$key)
  expect_equal(p$fraction, 1)
  expect_equal(p$message, "passo 3")
})

test_that("duração real é medida e reportada", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(list(op = "add_node", type = "t/const", id = "c")))
  ev <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev$sink)
  d <- Filter(function(e) e$type == "done", ev$get())[[1]]
  expect_true(is.numeric(d$duration))
  expect_gte(d$duration, 0)
})

test_that("gc preserva o que o plano alcança", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(list(op = "add_node", type = "t/const", id = "c", params = list(v = 1))))
  tr_run(doc, registry = reg, store = s)
  doc2 <- tr_doc_apply(doc, list(op = "set_param", node = "c", name = "v", value = 2), reg)
  tr_run(doc2, registry = reg, store = s)

  keep <- tr_plan_keys(tr_plan(doc2, registry = reg, store = s))
  expect_equal(tr_store_gc(s, keep = keep, max_age_days = 0), 1L)  # a chave velha sai
  expect_equal(tr_value(doc2, "c", reg, s)$v, 2)                   # a atual continua
})

test_that("pool exige mirai e falha classificado quando ausente", {
  skip_if(requireNamespace("mirai", quietly = TRUE), "mirai instalado")
  expect_error(tr_executor_pool(2), class = "tr_error_missing_mirai")
})

# --- Regressões da revisão de E4 -------------------------------------------

#' Executor assíncrono falso: capacidade N, e o resultado só fica pronto
#' depois de `delay` polls. Sem isto, TODO o caminho de `inflight`/polling
#' ficava sem um único teste — foi por aí que o bug de ordem passou.
fake_async <- function(cap = 2L, delay = 2L) {
  structure(list(
    kind = "fake", capacity = function() cap,
    submit = function(unit, registry, store) {
      # `force(unit)` NÃO é enfeite: `unit` chega como PROMESSA presa à
      # variável `u` do while de despacho do scheduler, que é reatribuída a
      # cada unidade pronta dentro da MESMA chamada (duas unidades do mesmo
      # nível, como os dois lados de um diamante, cabem no mesmo laço). Sem
      # forçar aqui, `e$run()` só resolve a promessa horas depois — e nesse
      # ponto `u` já foi sobrescrita pela ÚLTIMA unidade despachada naquele
      # laço, e as duas closures acabam rodando a MESMA unidade errada.
      force(unit)
      e <- new.env(parent = emptyenv()); e$left <- delay
      e$run <- function() .tr_capture_unit(unit, registry, store)
      e
    },
    collect = function(token) {
      token$left <- token$left - 1L
      if (token$left > 0L) return(NULL)
      token$run()
    },
    cancel = function(tokens) invisible(TRUE),
    shutdown = function() invisible(TRUE)
  ), class = "tr_executor")
}

test_that("com capacidade > 1, consumidor nunca é despachado antes do produtor", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "c", params = list(v = 3)),
    list(op = "add_node", type = "t/inc",   id = "i", params = list(by = 4)),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "i", to_port = "x")))

  ev <- events_of()
  tr_run(doc, registry = reg, store = s, executor = fake_async(2L, 3L), on_event = ev$sink)
  t <- vapply(ev$get(), function(e) paste0(e$node %||% "-", ":", e$type), "")
  expect_false(any(grepl("failed", t)))
  # `i` só pode começar depois de `c` terminar.
  expect_lt(which(t == "c:done"), which(t == "i:running"))
  expect_equal(tr_store_get(s, tr_plan(doc, registry = reg)$units$i$outputs$out,
                            tr_get_type("t/box", reg))$v, 7)
})

test_that("DAG diamante roda correto e em paralelo", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "a", params = list(v = 10)),
    list(op = "add_node", type = "t/inc",   id = "l", params = list(by = 1)),
    list(op = "add_node", type = "t/inc",   id = "r", params = list(by = 2)),
    list(op = "add_node", type = "t/sum",   id = "m"),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "l", to_port = "x"),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "r", to_port = "x"),
    list(op = "connect", from_node = "l", from_port = "out", to_node = "m", to_port = "xs"),
    list(op = "connect", from_node = "r", from_port = "out", to_node = "m", to_port = "xs")))
  ev <- events_of()
  tr_run(doc, registry = reg, store = s, executor = fake_async(2L, 2L), on_event = ev$sink)
  expect_false(any(grepl("failed|blocked", types_of(ev$get()))))
  expect_equal(tr_value(doc, "m", reg, s)$v, 23)   # (10+1) + (10+2)
})

test_that("falha bloqueia o jusante TRANSITIVO, sem gravar erro falso", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "c"),
    list(op = "add_node", type = "t/boom",  id = "b"),
    list(op = "add_node", type = "t/inc",   id = "i"),
    list(op = "add_node", type = "t/inc",   id = "j"),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "b", to_port = "x"),
    list(op = "connect", from_node = "b", from_port = "out", to_node = "i", to_port = "x"),
    list(op = "connect", from_node = "i", from_port = "out", to_node = "j", to_port = "x")))

  ev <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev$sink)
  t <- vapply(ev$get(), function(e) paste0(e$node %||% "-", ":", e$type), "")
  expect_true("b:failed"  %in% t)
  expect_true("i:blocked" %in% t)
  expect_true("j:blocked" %in% t)   # o NETO também
  expect_false("j:running" %in% t)

  # E nenhum erro falso foi cacheado: `j` continua bloqueado, não "failed".
  p <- tr_plan(doc, registry = reg, store = s)
  expect_false(isTRUE(p$units$j$failed))
  ev2 <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev2$sink)
  t2 <- vapply(ev2$get(), function(e) paste0(e$node %||% "-", ":", e$type), "")
  expect_true("j:blocked" %in% t2)
  expect_false(any(grepl(":running", t2)))
})

test_that("nó que devolve NULL não vira 'argumento ausente' no consumidor", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(
      tr_node("t/nada", fn = function() NULL, description = "Não devolve nada: só NULL.",
              outputs = list(out = "t/box")),
      tr_node("t/ve",   fn = function(x) list(v = is.null(x)),
              description = "Diz se a entrada que chegou é NULL.",
              inputs = list(x = "t/box"), outputs = list(out = "t/box")))), registry = reg)
  s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/nada", id = "n"),
    list(op = "add_node", type = "t/ve",   id = "u"),
    list(op = "connect", from_node = "n", from_port = "out", to_node = "u", to_port = "x")))
  expect_true(tr_value(doc, "u", reg, s)$v)
})

test_that("traceback aponta pro código que quebrou, não pro trama", {
  reg <- tr_registry()
  col <- local({
    fundo <- function() stop("lá no fundo")
    meio  <- function() fundo()
    tr_collection(id = "t", types = list(tr_type("t/box")),
      nodes = list(tr_node("t/fundo", fn = function() meio(),
                           description = "Chama uma cadeia de funções que estoura lá no fundo.",
                           outputs = list(out = "t/box"))))
  })
  tr_use(col, registry = reg); s <- tmp_store()
  doc <- build(reg, list(list(op = "add_node", type = "t/fundo", id = "n")))
  tr_run(doc, registry = reg, store = s)
  h <- tr_store_handle(s, tr_plan(doc, registry = reg)$units$n$outputs$out)
  expect_match(h$error$traceback, "meio")
  expect_match(h$error$traceback, "fundo")
})

test_that("evento de cache carrega handle — o card se pinta ao reabrir", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(list(op = "add_node", type = "t/const", id = "c", params = list(v = 7))))
  tr_run(doc, registry = reg, store = s)

  ev <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev$sink)
  e <- Filter(function(x) x$type == "cached", ev$get())[[1]]
  expect_equal(e$handles$out$preview$data$v, 7)
  expect_equal(e$handles$out$summary$v, 7)
  expect_equal(names(e$outputs), "out")     # mapa porta -> chave
})

test_that("run_finished fecha a execução com o balanço", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "c"),
    list(op = "add_node", type = "t/boom",  id = "b"),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "b", to_port = "x")))
  ev <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev$sink)
  fin <- ev$get()[[length(ev$get())]]
  expect_equal(fin$type, "run_finished")
  expect_true("c" %in% fin$done)
  expect_true("b" %in% fin$skipped)
})

test_that("tr_value funciona em nó volatile e recusa nó sem saída", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(
      tr_node("t/agora", fn = function() list(v = 1), outputs = list(out = "t/box"),
              description = "Devolve um, sem nunca reaproveitar cache.",
              volatile = TRUE),
      tr_node("t/fim", fn = function(x) invisible(NULL), inputs = list(x = "t/box"),
              description = "Consome a caixa e não devolve saída nenhuma."))),
    registry = reg)
  s <- tmp_store()
  doc <- build(reg, list(list(op = "add_node", type = "t/agora", id = "n")))
  expect_equal(tr_value(doc, "n", reg, s)$v, 1)

  doc2 <- build(reg, list(
    list(op = "add_node", type = "t/agora", id = "n"),
    list(op = "add_node", type = "t/fim",   id = "f"),
    list(op = "connect", from_node = "n", from_port = "out", to_node = "f", to_port = "x")))
  expect_error(tr_value(doc2, "f", reg, s), class = "tr_error_no_output")
  expect_error(tr_value(doc, "n", reg, s, port = "nope"), class = "tr_error_unknown_port")
})

test_that("nó sem porta de saída roda e cacheia", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(
      tr_node("t/c",   fn = function() list(v = 1), description = "Devolve uma caixa com o valor um.",
              outputs = list(out = "t/box")),
      tr_node("t/fim", fn = function(x) invisible(NULL), inputs = list(x = "t/box"),
              description = "Consome a caixa e não devolve saída nenhuma."))),
    registry = reg)
  s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/c",   id = "c"),
    list(op = "add_node", type = "t/fim", id = "f"),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "f", to_port = "x")))
  tr_run(doc, registry = reg, store = s)
  ev <- events_of(); tr_run(doc, registry = reg, store = s, on_event = ev$sink)
  expect_equal(types_of(ev$get()), c("cached", "cached"))
})

test_that("adaptador ausente no worker falha classificado", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t",
    types = list(tr_type("t/a"), tr_type("t/b")),
    nodes = list(tr_node("t/src", fn = function() 1, description = "Devolve o número um.",
                         outputs = list(out = "t/a")),
                 tr_node("t/dst", fn = function(x) x, inputs = list(x = "t/b"),
                         description = "Repassa a entrada, que precisa chegar no outro tipo.",
                         outputs = list(out = "t/b"))),
    adapters = list(tr_adapter("t/a", "t/b", function(v) v))), registry = reg)
  s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/src", id = "s"),
    list(op = "add_node", type = "t/dst", id = "d"),
    list(op = "connect", from_node = "s", from_port = "out", to_node = "d", to_port = "x")))
  plan <- tr_plan(doc, registry = reg, store = s)
  reg2 <- tr_registry()   # worker SEM o adaptador
  tr_use(tr_collection(id = "t", types = list(tr_type("t/a"), tr_type("t/b")),
    nodes = list(tr_node("t/src", fn = function() 1, description = "Devolve o número um.",
                         outputs = list(out = "t/a")),
                 tr_node("t/dst", fn = function(x) x, inputs = list(x = "t/b"),
                         description = "Repassa a entrada, que precisa chegar no outro tipo.",
                         outputs = list(out = "t/b")))), registry = reg2)
  tr_run_plan(plan, registry = reg, store = s)   # produz `s`
  r <- .tr_capture_unit(plan$units$d, reg2, s)
  expect_false(r$ok)
  expect_equal(r$error$class, "tr_error_unknown_adapter")
})

test_that("pool recusa coleção que não veio de pacote", {
  skip_if_not(requireNamespace("mirai", quietly = TRUE), "mirai ausente")
  expect_error(tr_executor_pool(2, registry = store_registry()),
               class = "tr_error_collection_not_dispatchable")
})

test_that(".ctx$path resolve contra a raiz do projeto, e o fingerprint olha o mesmo arquivo", {
  root <- tempfile("proj"); dir.create(root)
  writeLines("7", file.path(root, "n.txt"))
  s <- tr_store(file.path(root, ".trama", "store"), project_root = root)
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(tr_node("t/read", fn = function(path, .ctx) list(v = as.numeric(readLines(.ctx$path(path)))),
      description = "Lê um número do arquivo apontado pelo caminho.",
      outputs = list(out = "t/box"), params = list(path = tr_param_text("")),
      pure = FALSE,
      fingerprint = function(params, ctx) { i <- file.info(ctx$path(params$path)); paste0(i$size, ":", i$mtime) }))),
    registry = reg)
  doc <- build(reg, list(list(op = "add_node", type = "t/read", id = "r", params = list(path = "n.txt"))))
  withr::with_dir(tempdir(), {          # cwd DIFERENTE da raiz do projeto
    expect_equal(tr_value(doc, "r", registry = reg, store = s)$v, 7)
    Sys.sleep(1.1); writeLines("8", file.path(root, "n.txt"))
    expect_equal(tr_value(doc, "r", registry = reg, store = s)$v, 8)
  })
})

test_that("tr_value de nó INTERIOR de região erra nomeando a região", {
  # `plan$units[[node]]` é NULL para um membro interior — a região é uma unidade
  # só, nomeada pelo colapso. Sem consultar `plan$region_of`,
  # `length(u$output_types)` dava 0, o `sprintf` com `u$node_type = NULL`
  # devolvia `character(0)`, e o abort saía com mensagem VAZIA: quem pedisse o
  # valor de um nó de dentro da região não recebia pista nenhuma de por quê.
  # Regressão: em `9d9b9b3` cada nó interior tinha unidade própria.
  reg <- stream_registry(); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("acumula", "s/acumula", from = "fonte") |>
    tr_add("colapsa", "s/colapsa", from = "acumula"))

  err <- tryCatch(tr_value(doc, "acumula", reg, s), error = function(e) e)
  expect_s3_class(err, "tr_error_no_output")
  msg <- conditionMessage(err)
  expect_true(nzchar(msg))
  # A mensagem nomeia o nó pedido, a unidade que o executa, e o que pedir no
  # lugar: é o que transforma "não tem saída" em algo acionável.
  expect_match(msg, "'acumula'")
  expect_match(msg, "'colapsa'")

  # A FONTE da região também é membro interior: ela não grava artefato próprio,
  # e cai no mesmo ramo do `region_of`. (Não é o caso "nó fora do plano" — esse
  # nem chega aqui: `tr_plan()` aborta antes com `tr_error_unknown_node`.)
  err2 <- tryCatch(tr_value(doc, "fonte", reg, s), error = function(e) e)
  expect_s3_class(err2, "tr_error_no_output")
  expect_match(conditionMessage(err2), "'fonte'")
})
