# Parcial POR NÓ: o canal de progresso visto de dentro de uma região.
#
# É o canal que já existe (`<store>/progress/<chave>.json`, lido por polling no
# `collect()`), com um campo novo. Todo teste aqui existe contra um modo de
# falha CALADO do outro lado do canal: mapa de um nó só que atravessa o
# `auto_unbox` como escalar e perde o id (o bug que o comentário longo de
# `store.R` documenta, e que dava card em branco sem erro nenhum), nó cujo
# parcial é NULL e desaparece do mapa, evento `partial` sem o campo `node` (o
# front escreve o parcial do nó interior no card do colapso), e progresso velho
# de um run anterior lido como se fosse deste.

# Coleção com PREVIEW no tipo: `s/tab` (helper-collection.R) não tem preview, e
# sem preview no tipo não há como distinguir "usou o preview do tipo do ponto"
# de "não renderizou nada". Mora aqui, e não no helper, porque é a única coisa
# que estes testes pedem além do que a coleção de fluxo já dá.
progresso_collection <- function(e = new.env(parent = emptyenv())) {
  ponto <- function(...) tr_port("g/v", stream = TRUE, ...)
  tr_collection(
    id = "g", version = "1.0.0", label = "Progresso de fluxo",
    types = list(
      tr_type("g/v", label = "Valor",
              preview = function(x, ctx) tr_preview("g/v", data = list(v = x))),
      # Segundo tipo, SEM preview: um nó dele prova que o parcial sai do tipo do
      # ponto daquele nó, e não de um tipo só da região.
      tr_type("g/w", label = "Sem preview")),
    nodes = list(
      tr_node("g/fonte", fn = function(n) as.list(seq_len(n)),
              outputs = list(out = ponto()), params = list(n = tr_param_int(4L)),
              description = "Emite n pontos: 1..n."),
      # Espia o próprio canal de dentro do laço: o executor sequencial roda no
      # mesmo processo, então é o único jeito de ver o progresso ANTES de o
      # scheduler limpá-lo na conclusão.
      tr_node("g/dobro", fn = function(x) {
                e$vistos <- c(e$vistos, list(tr_progress(e$store, e$key)))
                x * 2
              },
              inputs = list(x = "g/v"), outputs = list(out = "g/v"),
              description = "Dobra o ponto e espia o progresso publicado."),
      tr_node("g/media", fn = function(x) x,
              inputs = list(x = ponto()), outputs = list(out = ponto()),
              init = function() list(soma = 0, n = 0L),
              step = function(state, x) {
                state$soma <- state$soma + x; state$n <- state$n + 1L
                list(state = state, out = state$soma / state$n)
              },
              description = "Média corrente dos pontos."),
      tr_node("g/colapsa", fn = function(x) if (length(x)) unlist(x) else numeric(),
              inputs = list(x = ponto()), outputs = list(out = "g/v"),
              description = "Junta os pontos num histórico.")
    ))
}

progresso_registry <- function(e = new.env(parent = emptyenv())) {
  reg <- tr_registry(); tr_use(progresso_collection(e), registry = reg); reg
}

# Roda a região como o worker roda. `publish_every = 0` tira o estrangulamento
# de cadência: o teste não pode depender de relógio.
roda <- function(doc, reg, store, colapso = "co", publish_every = 0) {
  p <- tr_plan(doc, registry = reg, store = store)
  u <- p$units[[colapso]]
  expect_equal(u$kind, "stream_region")
  list(handles = .tr_run_unit(u, reg, store, ctx_extra = list(publish_every = publish_every)),
       unit = u)
}

# --- `.ctx$partial_node()`: o campo novo -------------------------------------

test_that("parcial de UM nó só sobrevive ao auto_unbox com o id do nó", {
  # O modo de falha: `auto_unbox = TRUE` desembrulha vetor de comprimento 1 e
  # joga o NOME fora. Um mapa de um nó só é exatamente esse caso, e é o caso
  # COMUM (região de um acumulador). Se o id se perder, o front recebe um
  # parcial que não sabe de quem é — card em branco, sem erro nenhum.
  reg <- progresso_registry(); s <- tmp_store()
  u <- list(node = "co", key = "k1", outputs = list(out = "o1"))
  ctx <- .tr_make_ctx(u, s)
  ctx$partial_node("ac", 7, tr_get_type("g/v", reg))

  p <- tr_progress(s, "k1")
  expect_type(p$nodes, "list")
  expect_named(p$nodes, "ac")
  expect_equal(p$nodes$ac$node, "ac")
  expect_true(isTRUE(p$nodes$ac$partial))
  expect_equal(p$nodes$ac$preview$data$v, 7)
})

test_that("mapa de vários nós mantém um id por nó, na ordem em que foram publicados", {
  reg <- progresso_registry(); s <- tmp_store()
  ctx <- .tr_make_ctx(list(node = "co", key = "k2", outputs = list()), s)
  ty <- tr_get_type("g/v", reg)
  ctx$partial_node("fo", 1, ty); ctx$partial_node("ac", 2, ty); ctx$partial_node("fo", 3, ty)

  p <- tr_progress(s, "k2")
  expect_named(p$nodes, c("fo", "ac"))
  expect_equal(p$nodes$fo$preview$data$v, 3)   # republicar substitui, não duplica
  expect_equal(p$nodes$ac$preview$data$v, 2)
})

test_that("parcial de nó NÃO derruba o parcial nem o progresso da unidade", {
  # Compatibilidade: o resto do sistema (todo nó longo comum) continua usando o
  # campo antigo, e o `collect()` lê os dois do mesmo arquivo.
  reg <- progresso_registry(); s <- tmp_store()
  u <- list(node = "co", key = "k3", outputs = list(),
            partial_type = tr_get_type("g/v", reg))
  ctx <- .tr_make_ctx(u, s)
  ctx$progress(0.5, "meio")
  ctx$partial(11)
  ctx$partial_node("ac", 22, tr_get_type("g/v", reg))

  p <- tr_progress(s, "k3")
  expect_equal(p$fraction, 0.5)
  expect_equal(p$message, "meio")
  expect_equal(p$partial$preview$data$v, 11)
  expect_equal(p$nodes$ac$preview$data$v, 22)

  # E o caminho inverso: publicar o parcial da unidade depois não apaga o mapa.
  ctx$partial(33)
  p2 <- tr_progress(s, "k3")
  expect_equal(p2$partial$preview$data$v, 33)
  expect_named(p2$nodes, "ac")
})

test_that("nó cujo valor do passo é NULL continua no mapa", {
  # `nodes[[id]] <- NULL` REMOVE o elemento. Um nó que legitimamente devolve
  # NULL num passo desapareceria do mapa, e o card dele ficaria com o parcial do
  # passo anterior — dado velho, calado.
  reg <- progresso_registry(); s <- tmp_store()
  ctx <- .tr_make_ctx(list(node = "co", key = "k4", outputs = list()), s)
  ctx$partial_node("nu", NULL, tr_get_type("g/v", reg))

  p <- tr_progress(s, "k4")
  expect_named(p$nodes, "nu")
})

test_that("tipo sem preview publica o nó sem preview, e não um erro", {
  reg <- progresso_registry(); s <- tmp_store()
  ctx <- .tr_make_ctx(list(node = "co", key = "k5", outputs = list()), s)
  ctx$partial_node("w", 1, tr_get_type("g/w", reg))
  p <- tr_progress(s, "k5")
  expect_named(p$nodes, "w")
  expect_null(p$nodes$w$preview)
})

# --- O driver publica, e `wants_ctx` passa a ser verdade ---------------------

test_that("durante a região, tr_progress() na chave dela devolve algo", {
  # O critério que a Fase 3 declarou (`wants_ctx = TRUE`) e ninguém honrava: o
  # despacho de `.tr_run_unit()` desvia pra `.tr_run_region()` antes do bloco do
  # ctx, então a unidade mais demorada do grafo era a única a não publicar nada.
  e <- new.env(parent = emptyenv()); reg <- progresso_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "g/fonte", n = 4L) |>
    tr_add("du", "g/dobro", from = "fo") |>
    tr_add("co", "g/colapsa", from = "du"))
  p0 <- tr_plan(doc, registry = reg, store = s)
  expect_true(isTRUE(p0$units$co$wants_ctx))
  e$store <- s; e$key <- p0$units$co$key

  roda(doc, reg, s)
  # Quatro passos; o do passo 1 publica DEPOIS do `fn`, então o `fn` do passo 2
  # em diante vê progresso. Nenhum NULL a partir do segundo.
  expect_equal(length(e$vistos), 4L)
  expect_null(e$vistos[[1]])
  vivos <- e$vistos[-1]
  expect_true(all(vapply(vivos, function(p) !is.null(p), logical(1))))
  expect_true(all(vapply(vivos, function(p) !is.null(p$nodes), logical(1))))
})

test_that("o driver publica um parcial por nó do passo, com o preview do tipo do ponto", {
  e <- new.env(parent = emptyenv()); reg <- progresso_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "g/fonte", n = 3L) |>
    tr_add("ac", "g/media", from = "fo") |>
    tr_add("du", "g/dobro", from = "ac") |>
    tr_add("co", "g/colapsa", from = "du"))
  p0 <- tr_plan(doc, registry = reg, store = s)
  e$store <- s; e$key <- p0$units$co$key

  roda(doc, reg, s)
  ultimo <- e$vistos[[length(e$vistos)]]
  # Fonte, nó com memória e nó elevado — cada um com o parcial do PRÓPRIO passo.
  # O colapso não: o `fn` dele só roda depois do laço.
  expect_setequal(names(ultimo$nodes), c("fo", "ac", "du"))
  expect_false("co" %in% names(ultimo$nodes))
  # Passo 3 visto de dentro do `fn` do passo 3: o publicado é o do passo 2.
  expect_equal(ultimo$nodes$fo$preview$data$v, 2)
  expect_equal(ultimo$nodes$ac$preview$data$v, mean(1:2))
  expect_equal(ultimo$nodes$du$preview$data$v, 2 * mean(1:2))
  # E a fração do passo, no mesmo arquivo: é a barra de progresso da região.
  expect_equal(ultimo$fraction, 2 / 3)
})

test_that("cadência estrangulada publica menos que um por passo, e o primeiro sempre", {
  # O laço roda uma vez por PONTO; publicar todo passo de uma região de dez mil
  # pontos escreveria o JSON dez mil vezes, e o coordenador só lê a cada ~50ms.
  # O piso é o passo 1: sem ele o card ficaria em branco até o primeiro intervalo.
  e <- new.env(parent = emptyenv()); reg <- progresso_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "g/fonte", n = 6L) |>
    tr_add("du", "g/dobro", from = "fo") |>
    tr_add("co", "g/colapsa", from = "du"))
  p0 <- tr_plan(doc, registry = reg, store = s)
  e$store <- s; e$key <- p0$units$co$key

  roda(doc, reg, s, publish_every = 3600)
  expect_null(e$vistos[[1]])
  # Publicou no passo 1 (o piso) e em nenhum outro: o valor visto é o mesmo nos
  # cinco passos seguintes.
  vs <- vapply(e$vistos[-1], function(p) p$nodes$fo$preview$data$v, 0)
  expect_equal(vs, rep(1, 5))
})

test_that("região cacheada não publica nada", {
  # O parcial é VIVO: nada dele vai a chave do store. É o que sustenta a chave
  # da região omitir a impressão dos tipos das portas interiores.
  e <- new.env(parent = emptyenv()); reg <- progresso_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "g/fonte", n = 3L) |>
    tr_add("du", "g/dobro", from = "fo") |>
    tr_add("co", "g/colapsa", from = "du"))
  e$store <- s; e$key <- tr_plan(doc, registry = reg, store = s)$units$co$key
  roda(doc, reg, s)
  u <- tr_plan(doc, registry = reg, store = s)$units$co
  expect_true(isTRUE(u$cached))
  # Nenhuma chave nova além das saídas do colapso: `nodes` não virou artefato.
  handles <- list.files(file.path(s$root, "handles"))
  expect_equal(handles, paste0(unlist(u$outputs), ".json"))
})

# --- O scheduler emite um `partial` por nó ----------------------------------

test_that("collect() emite um `partial` por nó que mudou, com o campo node", {
  reg <- store_registry(); s <- tmp_store(); ev <- list()
  ex <- fake_async_executor(ticks = 4L)
  doc <- build(reg, list(list(op = "add_node", type = "t/const", id = "c")))
  plan <- tr_plan(doc, registry = reg, store = s)
  sch <- tr_scheduler(plan, reg, s, ex, function(e) ev[[length(ev) + 1]] <<- e)
  u <- plan$units$c
  sch$step()
  ctx <- .tr_make_ctx(u, s)
  ty <- tr_get_type("t/box", reg)
  ctx$partial_node("interior", list(v = 1), ty)
  sch$step()
  ctx$partial_node("outro", list(v = 2), ty)
  sch$step()
  while (!sch$step()) NULL

  ps <- Filter(function(x) identical(x$type, "partial"), ev)
  nós <- vapply(ps, function(x) x$node, "")
  # Um por nó, e o `node` é o id do nó INTERIOR — não o do colapso. Sem isso o
  # front escreveria os dois parciais no mesmo card.
  expect_equal(nós, c("interior", "outro"))
  expect_equal(ps[[1]]$handle$preview$data$v, 1)
  expect_equal(ps[[2]]$handle$preview$data$v, 2)
  # E `node` aparece UMA vez no evento: duas chaves com o mesmo nome no JSON
  # fariam o front ler a primeira, que é a do colapso.
  expect_equal(sum(names(ps[[2]]) == "node"), 1L)
})

test_that("o scheduler limpa o mapa de nós ao concluir, e não lê o de um run anterior", {
  reg <- store_registry(); s <- tmp_store(); ev <- list()
  doc <- build(reg, list(list(op = "add_node", type = "t/const", id = "c")))
  plan <- tr_plan(doc, registry = reg, store = s)
  u <- plan$units$c
  # Progresso ÓRFÃO: sobra de um run que morreu sem limpar (worker morto). Sem
  # limpeza no despacho, o primeiro poll do run novo emitiria o parcial velho
  # como se fosse deste — dado velho, com a autoridade de dado novo.
  .tr_make_ctx(u, s)$partial_node("fantasma", list(v = 99), tr_get_type("t/box", reg))
  ex <- fake_async_executor(ticks = 3L)
  sch <- tr_scheduler(plan, reg, s, ex, function(e) ev[[length(ev) + 1]] <<- e)
  while (!sch$step()) NULL

  expect_false(any(vapply(ev, function(x) identical(x$type, "partial"), logical(1))))
  expect_false(file.exists(file.path(s$root, "progress", paste0(u$key, ".json"))))
})
