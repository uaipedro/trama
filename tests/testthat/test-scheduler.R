# O scheduler é a execução como máquina de passos: o que `tr_run_plan()` fazia
# num `while`, agora é `step()` chamável de fora — pelo laço headless ou pelo
# `later` do Shiny. Estes testes cobrem o que o laço antigo NÃO podia fazer.

chain_doc <- function(reg) build(reg, list(
  list(op = "add_node", type = "t/const", id = "a", params = list(v = 1)),
  list(op = "add_node", type = "t/inc",   id = "b", params = list(by = 1)),
  list(op = "add_node", type = "t/inc",   id = "c", params = list(by = 1)),
  list(op = "connect", from_node = "a", from_port = "out", to_node = "b", to_port = "x"),
  list(op = "connect", from_node = "b", from_port = "out", to_node = "c", to_port = "x")))

test_that("step() avança um passo por vez e termina com run_finished", {
  reg <- store_registry(); s <- tmp_store(); ev <- list()
  sch <- tr_scheduler(tr_plan(chain_doc(reg), registry = reg, store = s), reg, s,
                      fake_async_executor(ticks = 2L), on_event = function(e) ev[[length(ev) + 1]] <<- e)
  expect_false(sch$finished())
  n <- 0L
  while (!sch$step()) { n <- n + 1L; expect_lt(n, 50L) }
  expect_true(sch$finished())
  types <- vapply(ev, function(e) e$type, "")
  expect_equal(tail(types, 1), "run_finished")
  expect_equal(sum(types == "done"), 3L)
  # `results[[node]]` guarda o HANDLE por porta (o que `tr_store_put` devolve),
  # não a chave nua — `$key` é o campo que `tr_store_get` espera.
  expect_equal(tr_store_get(s, sch$result()$results$c$out$key, tr_get_type("t/box", reg))$v, 3)
})

test_that("tr_run_plan continua dando o mesmo resultado do laço antigo", {
  reg <- store_registry(); s <- tmp_store()
  r <- tr_run_plan(tr_plan(chain_doc(reg), registry = reg, store = s), reg, s)
  expect_setequal(r$done, c("a", "b", "c")); expect_length(r$skipped, 0)
})

test_that("handoff cancela o que saiu do plano e o sucessor adota o que ficou", {
  reg <- store_registry(); s <- tmp_store(); ex <- fake_async_executor(ticks = 3L, capacity = 3L)
  doc1 <- chain_doc(reg)
  ev <- list(); rec <- function(e) ev[[length(ev) + 1]] <<- e
  s1 <- tr_scheduler(tr_plan(doc1, registry = reg, store = s), reg, s, ex, rec, run_id = "r1")
  s1$step()                                   # despacha `a` (único pronto)
  expect_equal(ex$log$submitted, "a")
  # Edita `c`: a chave de `a` e `b` não muda; a de `c` sim.
  doc2 <- tr_doc_apply(doc1, list(op = "set_param", node = "c", name = "by", value = 5), reg)
  plan2 <- tr_plan(doc2, registry = reg, store = s)
  survivors <- s1$handoff(unlist(plan2$keys))
  expect_length(survivors, 1L)                # `a` sobrevive: mesma chave
  expect_true(s1$finished())
  expect_false("run_finished" %in% vapply(ev, function(e) e$type, ""))
  s2 <- tr_scheduler(plan2, reg, s, ex, rec, run_id = "r2", inherit = survivors)
  while (!s2$step()) NULL
  expect_equal(ex$log$submitted, c("a", "b", "c"))   # `a` NÃO foi redespachado
  adopted <- Filter(function(e) identical(e$type, "running") && isTRUE(e$adopted), ev)
  expect_length(adopted, 1L); expect_equal(adopted[[1]]$node, "a")
  expect_equal(tr_store_get(s, s2$result()$results$c$out$key, tr_get_type("t/box", reg))$v, 7)
})

test_that("handoff cancela unidade em voo cuja chave saiu do plano", {
  reg <- store_registry(); s <- tmp_store(); ex <- fake_async_executor(ticks = 3L)
  doc1 <- build(reg, list(list(op = "add_node", type = "t/const", id = "a", params = list(v = 1))))
  ev <- list(); rec <- function(e) ev[[length(ev) + 1]] <<- e
  s1 <- tr_scheduler(tr_plan(doc1, registry = reg, store = s), reg, s, ex, rec)
  s1$step()
  doc2 <- tr_doc_apply(doc1, list(op = "set_param", node = "a", name = "v", value = 2), reg)
  survivors <- s1$handoff(unlist(tr_plan(doc2, registry = reg, store = s)$keys))
  expect_length(survivors, 0L)
  expect_equal(ex$log$cancelled, "a")
  expect_true("cancelled" %in% vapply(ev, function(e) e$type, ""))
})

test_that("progresso publicado pelo .ctx vira evento durante o run", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/box")),
    nodes = list(tr_node("t/slow", fn = function(.ctx) { .ctx$progress(0.5, "meio"); list(v = 1) },
                         description = "Publica metade do progresso e devolve um.",
                         outputs = list(out = "t/box")))), registry = reg)
  s <- tmp_store(); ev <- list()
  # `collect()` só relê o progresso do store no ramo "ainda rodando" (`res`
  # NULL) — no tique em que o executor finalmente resolve, o scheduler vai
  # direto pro `done` e não relê. Por isso `ticks = 3L`: um tique de sobra
  # entre a escrita do progresso e o tique final, pra existir uma coleta
  # "ainda rodando" que o veja antes de a unidade terminar.
  ex <- fake_async_executor(ticks = 3L)
  doc <- build(reg, list(list(op = "add_node", type = "t/slow", id = "s")))
  plan <- tr_plan(doc, registry = reg, store = s)
  sch <- tr_scheduler(plan, reg, s, ex, function(e) ev[[length(ev) + 1]] <<- e)
  sch$step()                                   # despacha; tique 1 -> NULL
  .tr_make_ctx(plan$units$s, s)$progress(0.25, "um quarto")
  sch$step()                                   # tique 2 -> NULL, vê o progresso
  while (!sch$step()) NULL                     # tique 3 -> termina
  pr <- Filter(function(e) identical(e$type, "progress"), ev)
  expect_gte(length(pr), 1L); expect_equal(pr[[1]]$fraction, 0.25)
  expect_false(file.exists(file.path(s$root, "progress", paste0(plan$units$s$key, ".json"))))
})

test_that(".ctx$partial publica preview parcial e o handle final substitui", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t",
    types = list(tr_type("t/box", preview = function(x, ctx) tr_preview("t/box", data = list(v = x$v)))),
    nodes = list(tr_node("t/slow", fn = function(.ctx) { .ctx$partial(list(v = 0.5)); list(v = 1) },
                         description = "Publica um preview parcial antes de devolver um.",
                         outputs = list(out = "t/box")))), registry = reg)
  # Mesma ressalva do teste de progresso: `collect()` só relê o store no ramo
  # "ainda rodando" — precisa de um tique de sobra entre escrever o parcial e
  # o tique final que resolve a unidade.
  s <- tmp_store(); ev <- list(); ex <- fake_async_executor(ticks = 3L)
  doc <- build(reg, list(list(op = "add_node", type = "t/slow", id = "s")))
  plan <- tr_plan(doc, registry = reg, store = s)
  sch <- tr_scheduler(plan, reg, s, ex, function(e) ev[[length(ev) + 1]] <<- e)
  sch$step()
  u <- plan$units$s; u$partial_type <- tr_get_type("t/box", reg)
  .tr_make_ctx(u, s)$partial(list(v = 0.5))
  sch$step()
  while (!sch$step()) NULL
  types <- vapply(ev, function(e) e$type, "")
  expect_true("partial" %in% types)
  p <- ev[[which(types == "partial")[[1]]]]
  expect_equal(p$handle$preview$data$v, 0.5)
  expect_true(which(types == "done") > which(types == "partial")[[1]])
})
