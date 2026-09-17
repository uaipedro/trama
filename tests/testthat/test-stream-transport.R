# O contrato entre o PAYLOAD do motor e os campos que o front lê — a Fase 8
# não tem harness de React (só os dois pontos puros de `params.js`, cobertos
# em `tests/js/params.test.mjs`), então é aqui que uma mudança de forma no
# motor tem que quebrar um teste, e não um card em branco em produção. É o
# mesmo argumento do comentário longo de `store.R` (o `files` que perdeu o
# nome, lido como "p", sem erro nenhum) aplicado ao canal novo desta fase:
# `.tr_plan_regions()` (R/plan.R) e os eventos de unidade de uma região
# (R/scheduler.R) por onde `inst/www/editor.js` decide em qual card pintar
# cada evento.
#
# Cada teste nomeia, no comentário, qual leitura do front ele sustenta — pra
# quem mexer no motor achar rápido o que vai quebrar.

regiao_dupla_doc <- function(reg) tr_flow_doc(tr_flow(reg) |>
  tr_add("fo", "s/pontos", n = 4L) |>
  tr_add("ac", "s/acumula", from = "fo") |>
  tr_add("c1", "s/colapsa", from = "ac") |>
  tr_add("c2", "s/colapsa", from = "ac"))

# --- `.tr_plan_regions()`: o que `R/transport.R` manda na mensagem `regions` -

test_that("plan_regions expõe exatamente os campos que App.jsx lê da mensagem 'regions'", {
  # `regionsRef.current[r.unit]` (editor.js) e dali `.id` (a CHAVE que
  # `tr_stream_cmd` espera), `.members`, `.roles[id]` e `.outputs[colapso]`.
  reg <- stream_registry()
  doc <- regiao_dupla_doc(reg)
  plan <- tr_plan(doc, registry = reg)
  rs <- trama:::.tr_plan_regions(plan)

  expect_length(rs, 1L)
  r <- rs[[1]]
  expect_named(r, c("id", "unit", "members", "roles", "outputs"))
  # `id` é a CHAVE da unidade — o que `tr_stream_command(store, key, cmd)`
  # espera, e o mesmo valor sob o qual `progress`/`partial` chegam.
  expect_equal(r$id, plan$units$c1$key)
  # `unit` é o nó que carrega os eventos: o PRIMEIRO colapso, em ordem de
  # `region$collapse` (que já sai ordenado — ver `.tr_stream_split()`).
  expect_equal(r$unit, "c1")
  # `expect_equal`, não `expect_setequal`: `members` é `region$order`, ordem
  # TOPOLÓGICA (comentário de `.tr_plan_regions()` acima), e é nessa ordem que
  # o contorno do card e o front em geral confiam implicitamente. Com
  # `expect_setequal` a suíte inteira passava mesmo invertendo a ordem
  # topológica — achado auditando este arquivo (Fase 8, revisão).
  expect_equal(unclass(r$members), c("fo", "ac", "c1", "c2"))
  expect_equal(r$roles$fo, "source")
  expect_equal(r$roles$ac, "lifted")
  expect_equal(r$roles$c1, "collapse")
  expect_equal(r$roles$c2, "collapse")
  # Dois colapsos: cada um com o(s) PRÓPRIO(S) nome(s) de saída, e são esses
  # nomes — não "out" — que o front usa pra achar o handle do SEGUNDO colapso
  # dentro do mapa `handles` do evento `done` (que vem nomeado pela região
  # inteira). `.tr_region_out_name()` qualifica com `:` quando há mais de um
  # colapso.
  expect_named(r$outputs, c("c1", "c2"))
  expect_equal(unclass(r$outputs$c1), "c1:out")
  expect_equal(unclass(r$outputs$c2), "c2:out")
})

test_that("a mensagem 'regions' serializa como ARRAY mesmo com uma única região", {
  # Achado RODANDO o app de verdade (Tarefa 8.1 — ver o relatório da fase):
  # `plan$units` é lista NOMEADA pelo id da unidade, `Filter()` preserva os
  # nomes, e uma lista NOMEADA de listas vira OBJETO no JSON
  # (`{"c1": {...}}`), não ARRAY (`[{...}]`). O front faz
  # `(m.regions || []).forEach(...)` (editor.js) — com um objeto no lugar do
  # array, `forEach` não existe, e o handler de TODA mensagem `tr_event`
  # morre (é um `addCustomMessageHandler` só, então o erro para o app inteiro
  # de reagir a qualquer evento seguinte). Com uma coisa a mais grave: com
  # SÓ uma região no documento — o caso mais comum — é exatamente quando o
  # bug aparece; com duas ou mais ele não aparece, porque aí a lista nomeada
  # também não colapsa. Por isso o teste força deliberadamente o caso de
  # UMA região só.
  reg <- stream_registry()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "s/pontos", n = 3L) |>
    tr_add("ac", "s/acumula", from = "fo") |>
    tr_add("c1", "s/colapsa", from = "ac"))
  plan <- tr_plan(doc, registry = reg)
  rs <- trama:::.tr_plan_regions(plan)
  expect_length(rs, 1L)
  json <- jsonlite::toJSON(list(regions = rs), auto_unbox = TRUE)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  # A prova de que é ARRAY, não objeto: `is.null(names(...))` — um objeto JSON
  # vira lista NOMEADA em R; um array vira lista SEM nomes.
  expect_null(names(parsed$regions))
  expect_length(parsed$regions, 1L)
})

test_that("plan_regions some quando não há região — front não monta contorno nem controle", {
  reg <- test_registry()
  doc <- add(tr_doc(), reg, "t/const")
  plan <- tr_plan(doc, registry = reg)
  expect_length(trama:::.tr_plan_regions(plan), 0L)
})

# PROVA DE MUTAÇÃO 1: se `.tr_plan_regions()` mandasse o id do nó sob o nome
# errado, o teste acima morre. Mutação real, executada e revertida, registrada
# no relatório da fase:
#
#   - out <- list(id = u$key, unit = u$node, ...)
#   + out <- list(id = u$key, NODE = u$node, ...)   # renomeado
#
#   Rodando só este arquivo com a troca:
#   -- Failure (test-stream-transport.R:29:3): plan_regions expõe... --
#   names(r) not equal to c("id", "unit", "members", "roles", "outputs").
#   Lengths differ: 5 is not 5. names(r)[2]: "NODE"  target[2]: "unit"
#
#   E o `expect_equal(r$unit, "c1")` morre em seguida com "r$unit is NULL".
#   A troca foi desfeita depois de confirmar a queda — não fica no código.

# --- Eventos de UNIDADE de uma região, pelo scheduler real --------------------

test_that("evento 'done' de região com dois colapsos nomeia os handles pelos nomes de plan_regions$outputs", {
  # `applyUnit`/`regionMemberPatch` (editor.js) resolvem o handle do SEGUNDO
  # colapso assim: `regiao.outputs[id].map((n) => m.handles[n]).find(Boolean)`.
  # Se o motor nomear os handles de outro jeito, a busca do front dá `null`
  # em silêncio — sem erro, card vazio. Este teste ata os dois lados.
  reg <- stream_registry(); s <- tmp_store()
  doc <- regiao_dupla_doc(reg)
  plan <- tr_plan(doc, registry = reg, store = s)
  saidas <- trama:::.tr_plan_regions(plan)[[1]]$outputs

  ev <- list()
  tr_run_plan(plan, reg, s, on_event = function(e) ev[[length(ev) + 1L]] <<- e)
  done <- Filter(function(x) identical(x$type, "done"), ev)
  expect_length(done, 1L)
  d <- done[[1]]
  # Campos que `unitState()` (editor.js) lê no ramo "done": duration, handles.
  # `node`/`key`/`outputs` são o que indexa o evento (`applyUnit`).
  expect_true(all(c("type", "run_id", "node", "node_type", "key", "outputs",
                    "duration", "handles") %in% names(d)))
  expect_equal(d$node, "c1")
  for (cid in names(saidas)) {
    nomes <- unclass(saidas[[cid]])
    achou <- any(vapply(nomes, function(n) !is.null(d$handles[[n]]), logical(1)))
    expect_true(achou, info = sprintf("colapso '%s': nenhum nome de %s achado em handles", cid,
                                      paste(nomes, collapse = ", ")))
  }
})

# PROVA DE MUTAÇÃO 2: se `plan.R` parasse de qualificar o nome de saída do
# segundo colapso (ou seja, `.tr_region_out_name()` sempre devolvesse a porta
# crua, mesmo com `multi = TRUE`), os dois colapsos gravariam sob a MESMA
# chave "out" — colidindo, o segundo pisando no primeiro em `outs[[nms[[i]]]]`.
# Mutação real, executada:
#
#   - .tr_region_out_name <- function(collapse, port, multi) {
#   -   if (!multi) return(port)
#   -   if (!nzchar(port)) collapse else paste0(collapse, ":", port)
#   - }
#   + .tr_region_out_name <- function(collapse, port, multi) port   # sempre a porta crua
#
#   Rodando com a troca (`.tr_plan_regions(plan)[[1]]$outputs` passa a dar
#   `list(c1 = "out", c2 = "out")`, os dois IGUAIS), o teste "plan_regions
#   expõe exatamente os campos..." já morre ANTES de chegar em
#   `tr_run_plan()`:
#
#   -- Failure (test-stream-transport.R:50:3): plan_regions expõe... --
#   Expected `unclass(r$outputs$c1)` to equal "c1:out".
#   actual:   "out"
#   expected: "c1:out"
#
#   A troca foi desfeita depois de confirmar a queda — não fica no código.

test_that("evento 'partial' de um MEMBRO interior chega com node = id do membro, não do colapso", {
  # `applyUnit` (editor.js) só sabe pintar o card certo porque `m.node` já é o
  # id do MEMBRO nesse evento — é o único evento em que `node` não é a unidade.
  reg <- stream_registry(); s <- tmp_store()
  doc <- regiao_dupla_doc(reg)
  plan <- tr_plan(doc, registry = reg, store = s)
  u <- plan$units$c1
  expect_equal(u$kind, "stream_region")

  ex <- fake_async_executor(ticks = 3L)
  ev <- list()
  sch <- tr_scheduler(plan, reg, s, ex, function(e) ev[[length(ev) + 1L]] <<- e)
  sch$step()  # despacha — "running" sai aqui
  ctx <- .tr_make_ctx(u, s)
  ty <- tr_get_type("s/tab", reg)
  ctx$partial_node("ac", list(v = 1), ty)
  sch$step()  # coleta o parcial órfão do tick anterior — ainda "em voo"
  while (!sch$step()) NULL

  parciais <- Filter(function(x) identical(x$type, "partial") && !is.null(x$node), ev)
  achou <- Filter(function(x) identical(x$node, "ac"), parciais)
  expect_length(achou, 1L)
  p <- achou[[1]]
  # A forma que `unitState()` lê no ramo "partial": `m.handle`.
  expect_true("handle" %in% names(p))
  # E a forma do HANDLE em si, que `.ctx$partial_node()` grava (worker.R) e
  # que a Fase 5 já fixou o formato: key/node/preview/partial. `Preview()`
  # (editor.js) lê `handle.preview`.
  expect_named(p$handle, c("key", "node", "preview", "partial"))
  expect_equal(p$handle$node, "ac")
  expect_true(isTRUE(p$handle$partial))
  # `node` aparece UMA vez no evento — chave repetida no JSON faria o front
  # ler a primeira (a do colapso), pintando o card errado. Já é o comportamento
  # coberto em test-stream-progress.R; repetido aqui como parte do MESMO
  # contrato que este arquivo existe pra travar.
  expect_equal(sum(names(p) == "node"), 1L)
})

test_that("mensagem de 'progress' de uma região bate no formato que contagemDoPasso() (params.js) espera", {
  # `contagemDoPasso()` em inst/www/params.js lê `progress.message` com
  # `/(\\d+)\\D+(\\d+)/` pra montar "passo 500 / 10.000" no card da fonte. O
  # texto vem de `sprintf("ponto %d de %d", i, n)` em R/stream-driver.R — se
  # ele mudar de forma (por exemplo pra "%d/%d" sem "de", ou pra só uma
  # fração), a extração do front não quebra alto: ela devolve `null` e o
  # contador do card da fonte simplesmente some, calado.
  #
  # ANTES este teste construía a string ele mesmo (`sprintf("ponto %d de %d",
  # 2L, 4L)`) e injetava via `ctx$progress()` direto — o comentário dizia
  # "publicada pelo driver real", mas não era: mutar `R/stream-driver.R:557`
  # pra "calculando" não derrubava nada aqui. Agora quem publica é o driver de
  # verdade (`.tr_run_unit()`, o mesmo caminho de produção, com
  # `publish_every = 0` pra não depender de relógio), e o teste só LÊ o que
  # sobrou em `progress/<chave>.json` — a mesma leitura que `tr_progress()`
  # documenta e que o scheduler faz por polling.
  reg <- stream_registry(); s <- tmp_store()
  doc <- regiao_dupla_doc(reg)
  plan <- tr_plan(doc, registry = reg, store = s)
  u <- plan$units$c1

  .tr_run_unit(u, reg, s, ctx_extra = list(publish_every = 0))
  prog <- tr_progress(s, u$key)
  expect_false(is.null(prog))
  msg <- prog$message
  # A MESMA regex de `contagemDoPasso()`, em R: dois grupos de dígitos.
  m <- regmatches(msg, regexec("(\\d+)\\D+(\\d+)", msg))[[1]]
  expect_length(m, 3L)
  # A fonte (`s/pontos`, n = 4L via `regiao_dupla_doc()`) publica um progresso
  # por passo com `publish_every = 0`; o último gravado é o do passo final.
  expect_equal(m[3], "4")
  expect_true("fraction" %in% names(prog))
})

# PROVA DE MUTAÇÃO 3: mutando `R/stream-driver.R:557` de
# `sprintf("ponto %d de %d", i, n)` pra `sprintf("calculando")` (mensagem sem
# dígito nenhum), rodando só este arquivo:
#
#   -- Failure (test-stream-transport.R): mensagem de 'progress'... --
#   `m <- regmatches(...)` tem comprimento 0, não 3.
#   Expected `length(m)` to equal 3.
#   actual:   0
#   expected: 3
#
# — o teste antigo (que fabricava a própria string) continuava verde com essa
# mesma mutação. A troca foi desfeita depois de confirmar a queda — não fica
# no código.

# --- O SERVIDOR manda o que R/plan.R calcula (cobertura de `run_now()`) -----

test_that("run_now() manda a mensagem 'regions' pelo barramento de verdade", {
  # Zero cobertura até aqui: nenhum teste passava por `tr_server()`/`run_now()`
  # pra ver se `send(\"regions\", ...)` (R/transport.R:103) de fato sai. Deletar
  # aquela linha quebraria toda a Fase 8 no navegador e a suíte inteira
  # continuaria verde — é a lacuna que o relatório de revisão aponta.
  reg <- stream_registry()
  root <- withr::local_tempdir("proj-regions")
  tr_project_new(root, character())
  proj <- tr_project_at(root, reg)
  tr_project_save(proj, regiao_dupla_doc(reg))

  shiny::testServer(tr_server(proj, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_ready = 1)

    regioes <- Filter(function(m) identical(m$type, "regions"), msgs)
    expect_length(regioes, 1L)
    rs <- regioes[[1]]$regions
    expect_length(rs, 1L)
    expect_equal(rs[[1]]$unit, "c1")
  })
})

# PROVA DE MUTAÇÃO 4: comentando a linha `send("regions", list(regions =
# .tr_plan_regions(plan)))` em `run_now()` (R/transport.R:103), rodando só
# este arquivo:
#
#   -- Failure (test-stream-transport.R): run_now() manda a mensagem... --
#   `regioes` tem comprimento 0, não 1.
#   Expected `length(regioes)` to equal 1.
#   actual:   0
#   expected: 1
#
# E o RESTO da suíte (`test_dir()` inteiro) continua 100% verde com a linha
# comentada — exatamente o achado do relatório de revisão ("deletar a linha
# quebra todo o front e a suíte inteira passa"). A mudança foi desfeita depois
# de confirmar a queda — não fica no código.

test_that("eventos de unidade chegam com 'unit_type' — o rótulo de que unitState() (editor.js) depende", {
  # Zero cobertura: `forward()` (R/transport.R:58) renomeia `ev$type` original
  # pra `ev$unit_type` e sobrescreve `ev$type` com "unit"/"run_finished". Anular
  # `unit_type` ali muda a FORMA que sai pro front (todo `switch` de
  # `unitState()` em editor.js lê `m.unit_type`) e nenhum teste do núcleo
  # percebe, porque nenhum olha pro campo.
  reg <- test_registry()
  root <- withr::local_tempdir("proj-unittype")
  tr_project_new(root, character())
  proj <- tr_project_at(root, reg)
  doc <- add(tr_doc(), reg, "t/const", id = "c1", value = 3)
  tr_project_save(proj, doc)

  shiny::testServer(tr_server(proj, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_ready = 1)

    # `pump()` some agenda via `later::later()`; aqui o scheduler é acessível
    # direto (mesmo objeto, `exec$sched`, no ambiente do servidor) — driblar o
    # laço do `later` e avançar sincronamente é o mesmo padrão de
    # test-scheduler.R ("step() avança um passo por vez").
    sched <- session$env$exec$sched
    expect_false(is.null(sched))
    n <- 0L
    while (!sched$step()) { n <- n + 1L; expect_lt(n, 200L) }

    unidades <- Filter(function(m) identical(m$type, "unit"), msgs)
    expect_true(length(unidades) >= 1L)
    expect_true(all(vapply(unidades, function(m) "unit_type" %in% names(m), logical(1))))
    tipos <- vapply(unidades, function(m) m$unit_type, "")
    expect_true("done" %in% tipos || "cached" %in% tipos)
    # E `type` nunca é o tipo cru do motor ("done"/"running"/...) — sempre
    # "unit" (ou "run_finished"), que é o que separa os dois `if` em
    # `applyUnit()` (editor.js).
    expect_true(all(vapply(unidades, function(m) m$type, "") == "unit"))
  })
})

# PROVA DE MUTAÇÃO 5: em `forward()` (R/transport.R:58), trocando
#
#   - ev$unit_type <- ev$type
#   + # (linha removida — unit_type nunca é gravado)
#
# rodando só este arquivo:
#
#   -- Failure (test-stream-transport.R): eventos de unidade chegam com... --
#   `all(vapply(unidades, function(m) "unit_type" %in% names(m), ...))` não é TRUE.
#
# E de novo, o resto da suíte passa inteiro com a mutação — nenhum outro teste
# olha pro campo `unit_type`. A mudança foi desfeita depois de confirmar a
# queda — não fica no código.
