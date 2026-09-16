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

# --- A região de fluxo vista pelo scheduler ---------------------------------
# A Fase 3 testou a região só pelo PLANO. Estes testes a atravessam pelo
# scheduler, que é onde o plano vira execução — e onde o fato de uma unidade
# ter mais de um id consumível aparece.

# fonte -> c1 ; fonte -> c2 ; c2 -> d2. Dois colapsos na MESMA região, e um
# consumidor do SEGUNDO: o grafo mínimo em que `u$node` (só o primeiro colapso)
# não basta.
regiao_dois_colapsos <- function(reg) {
  tr_flow_doc(
    tr_flow(reg) |>
      tr_add("fonte", "s/fonte") |>
      tr_add("c1", "s/colapsa", from = "fonte") |>
      tr_add("c2", "s/colapsa") |>
      tr_link("fonte:out", "c2:x") |>
      tr_add("d2", "s/mostra", from = "c2"))
}

test_that("região em cache libera o consumidor de QUALQUER colapso", {
  # `st$done` só recebia `u$node`, que numa região é o PRIMEIRO colapso. Quem
  # consome outro colapso esperava por um id que nunca chegava, ficava sem
  # ninguém em voo e saía com `blocked_by = "unreachable"` — motivo não
  # acionável, fora de `tr_plan_blocked()`, e o artefato dele nunca calculado
  # apesar de a região estar inteira no store.
  reg <- stream_registry(); s <- tmp_store()
  doc <- regiao_dois_colapsos(reg)
  u <- tr_plan(doc, registry = reg, store = s)$units[["c1"]]
  for (k in unlist(u$outputs)) {
    tr_store_put(s, k, 1, tr_get_type("s/tab", reg),
                 node_type = u$node_type, collections = u$collections)
  }

  p <- tr_plan(doc, registry = reg, store = s)
  expect_true(p$units$c1$cached)
  ev <- list()
  r <- tr_run_plan(p, reg, s, on_event = function(e) ev[[length(ev) + 1]] <<- e)
  expect_setequal(r$done, c("c1", "c2", "d2"))
  expect_length(r$skipped, 0L)
  expect_false("unreachable" %in% unlist(lapply(ev, function(e) e$blocked_by)))
})

# Executor que falha sempre, sem resolver nó nenhum: é o que permite exercitar
# o `prune()` sobre uma unidade-região na Fase 3, em que a região ainda não roda.
executor_que_falha <- function() structure(list(
  kind = "fake",
  capacity = function() 1L,
  submit = function(unit, registry, store) unit$node,
  collect = function(tok) list(ok = FALSE,
                               error = list(message = "explodiu", class = "tr_error_teste")),
  cancel = function(toks) invisible(TRUE),
  shutdown = function() invisible(TRUE)), class = "tr_executor")

test_that("região que falha poda o consumidor de QUALQUER colapso", {
  # O simétrico do cache: a poda andava pelos NOMES das unidades derrubadas, e o
  # nome da região é só o primeiro colapso — `d2` ficava pendente pra sempre
  # esperando por `c2`, e saía com "unreachable".
  #
  # TODO(fase-4): trocar esta montagem branca pelo driver de verdade, com um
  # membro cujo `step` explode — mesmo contrato, sem ficção.
  # O `kind` da unidade é trocado de propósito: é o único jeito de fazer a
  # região CHEGAR ao executor enquanto o andaime da Fase 3 a recusa antes do
  # despacho. O que está sob teste é o contrato do scheduler — unidade com
  # `region$collapse` pula e poda por TODOS os colapsos — e esse contrato não
  # depende de quem executa.
  reg <- stream_registry(); s <- tmp_store()
  p <- tr_plan(regiao_dois_colapsos(reg), registry = reg, store = s)
  p$units$c1$kind <- "node"

  ev <- list()
  r <- tr_run_plan(p, reg, s, executor_que_falha(),
                   on_event = function(e) ev[[length(ev) + 1]] <<- e)
  expect_setequal(r$skipped, c("c1", "c2", "d2"))
  expect_length(r$done, 0L)
  expect_false("unreachable" %in% unlist(lapply(ev, function(e) e$blocked_by)))
  bloqueio <- Filter(function(e) identical(e$node, "d2"), ev)[[1]]
  expect_equal(bloqueio$type, "blocked")
  expect_true("c2" %in% as.character(bloqueio$blocked_by))
})

test_that("poda ACUMULADA pela região: o consumidor do segundo colapso a dois saltos", {
  # O teste irmão acima cobre o frontier INICIAL da poda (a região é quem
  # falha). Este cobre o ACUMULADO: quem falha é um nó comum a montante, a
  # região entra na poda como vítima, e a rodada seguinte do `prune()` tem que
  # sair pelas SAÍDAS dela — todos os colapsos — pra alcançar `d2`.
  #
  # Sem isso a frente acumulava o NOME da unidade (só `c1`), `d2` consome `c2`,
  # não era alcançado, ficava pendente sem ninguém em voo e saía pelo
  # quebra-impasse com "unreachable". A perda era silenciosa: nenhum teste
  # falhava, porque nos grafos cobertos a região era a primeira a cair e o
  # frontier inicial já vinha certo.
  reg <- stream_registry(); s <- tmp_store()
  doc <- tr_flow_doc(
    tr_flow(reg) |>
      tr_add("tab", "s/tabela") |>
      tr_add("fonte", "s/fonte", from = "tab") |>
      tr_add("c1", "s/colapsa", from = "fonte") |>
      tr_add("c2", "s/colapsa") |>
      tr_link("fonte:out", "c2:x") |>
      tr_add("d2", "s/mostra", from = "c2"))
  p <- tr_plan(doc, registry = reg, store = s)
  p$units$c1$kind <- "node"   # mesma razão do teste acima: passar pelo andaime

  ev <- list()
  r <- tr_run_plan(p, reg, s, executor_que_falha(),
                   on_event = function(e) ev[[length(ev) + 1]] <<- e)
  expect_setequal(r$skipped, c("tab", "c1", "c2", "d2"))
  expect_false("unreachable" %in% unlist(lapply(ev, function(e) e$blocked_by)))
  bloqueio <- Filter(function(e) identical(e$node, "d2"), ev)[[1]]
  expect_equal(bloqueio$type, "blocked")
})

test_that("região é recusada até o driver existir (REMOVER na Fase 4)", {
  # ANDAIME — TODO(fase-4). A Fase 4 escreve o driver e apaga a guarda de
  # `tr_scheduler()`;
  # este teste tem que morrer com ela. Se ele começar a falhar porque a região
  # rodou, a guarda saiu — apague o teste, não o conserte.
  reg <- stream_registry(); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("c1", "s/colapsa", from = "fonte") |>
    tr_add("d1", "s/mostra", from = "c1"))
  ev <- list()
  tr_run(doc, registry = reg, store = s, on_event = function(e) ev[[length(ev) + 1]] <<- e)
  regiao <- Filter(function(e) identical(e$node, "c1"), ev)[[1]]
  expect_equal(regiao$type, "invalid")
  # A frase é lida pelo autor do documento: tem que dizer que falta feature no
  # trama, não que falta algo no grafo dele.
  expect_match(regiao$reason, "está correta")
  expect_match(regiao$reason, "ainda não sabe executá-la")
})

test_that("handle sob chave de região existe se, e só se, a região rodou", {
  # A INVARIANTE, escrita para sobreviver à Fase 4. A primeira versão afirmava
  # "nenhum handle, nunca" — verdade só enquanto o driver não existe, e o
  # implementador da Fase 4 encontraria um teste vermelho que se anuncia como
  # permanente. A reação mais barata a teste vermelho é enfraquecê-lo, e aí a
  # invariante morria justamente quando passava a valer. O que é permanente é a
  # CORRELAÇÃO: handle sob a chave da região se e só se a região emitiu `done`.
  # `.tr_run_unit()` não acha `trama/stream_region` no registro, e o `collect()`
  # gravava esse erro sob TODA chave de saída dela. A chave não muda quando o
  # driver chegar: o `failed = TRUE` envenenado sobrevivia à feature que faltava
  # e o primeiro uso do driver reportaria, do cache, um erro de quando ele não
  # existia. Para sempre — nenhum `tr_plan()` recalcula um handle que existe.
  reg <- stream_registry(); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("c1", "s/colapsa", from = "fonte") |>
    tr_add("d1", "s/mostra", from = "c1"))
  u <- tr_plan(doc, registry = reg, store = s)$units[["c1"]]
  ev <- list()
  tr_run(doc, registry = reg, store = s, on_event = function(e) ev[[length(ev) + 1]] <<- e)
  rodou <- any(vapply(ev, function(e) identical(e$node, "c1") &&
                        e$type %in% c("done", "cached"), logical(1)))

  for (k in unlist(u$outputs)) {
    if (rodou) expect_false(is.null(tr_store_handle(s, k)))
    else       expect_null(tr_store_handle(s, k))
  }
  # E, quando não rodou, o plano seguinte não pode vir envenenado do cache. Era
  # este o estrago: `.tr_run_unit()` não achava `trama/stream_region` no
  # registro e o `collect()` gravava esse erro sob TODA chave de saída da
  # região. A chave não muda quando o driver chegar, então o `failed = TRUE`
  # sobreviveria à feature que faltava, e o primeiro uso do driver reportaria,
  # do cache, um erro de quando ele não existia. Para sempre — nenhum
  # `tr_plan()` recalcula um handle que já existe.
  if (!rodou) {
    p2 <- tr_plan(doc, registry = reg, store = s)
    expect_false(isTRUE(p2$units$c1$failed))
    expect_null(p2$units$c1$handles)
  }
})

test_that("região a jusante de região sai UMA vez em skipped (REMOVER na Fase 4)", {
  # ANDAIME — TODO(fase-4), junto com a guarda de `tr_scheduler()`. A segunda região é podada
  # pela primeira e depois revisitada pelo retrato do `Filter`: sem a checagem
  # de `st$pending`, ela saía como "blocked" e de novo como "invalid", com o id
  # duplicado em `skipped` — o front acenderia e apagaria o mesmo card.
  reg <- stream_registry(); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("f1", "s/fonte") |>
    tr_add("c1", "s/colapsa", from = "f1") |>
    tr_add("f2", "s/fonte", from = "c1") |>
    tr_add("c2", "s/colapsa", from = "f2"))
  ev <- list()
  r <- tr_run_plan(tr_plan(doc, registry = reg, store = s), reg, s,
                   on_event = function(e) ev[[length(ev) + 1]] <<- e)
  expect_equal(sort(r$skipped), c("c1", "c2"))
  expect_equal(sum(vapply(ev, function(e) identical(e$node, "c2"), TRUE)), 1L)
})
