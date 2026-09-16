# O driver da região: o laço em lockstep. É o primeiro arquivo em que uma
# região de fluxo EXECUTA.
#
# Todo teste aqui existe contra um modo de falha do laço, e nenhum deles é
# ruidoso: nó elevado chamado uma vez em vez de N, estado que não atravessa o
# passo, entrada comum relida a cada ponto, colapso que vê um ponto em vez do
# fluxo. Nenhum desses erra alto — todos produzem um histórico plausível e
# errado, que vai pro store e é servido do cache pra sempre.

# Coleção paramétrica e INSTRUMENTADA: os nós registram cada chamada num
# ambiente que o teste lê depois do run. O executor sequencial roda no mesmo
# processo, então isso é o único jeito de provar "uma vez por ponto" sem abrir
# o driver — as asserções continuam sobre o CONTRATO, não sobre o código.
diario <- function() {
  e <- new.env(parent = emptyenv())
  e$puros <- list()      # cada chamada do `fn` elevado, com o que recebeu
  e$comuns <- list()     # o valor da entrada COMUM vista em cada passo
  e$steps <- list()      # (state, x) de cada `step`
  e$inits <- list()      # os params que cada `init` recebeu
  e$colapsos <- list()   # o que cada chamada do colapso recebeu
  e$restores <- 0L       # leituras de artefato do store, do tipo inteiro
  e
}

driver_collection <- function(e = diario()) {
  ponto <- function(...) tr_port("d/v", stream = TRUE, ...)

  # `restore` que conta: é a prova de que a entrada comum é lida UMA vez, e não
  # uma por passo. Um contador dentro do `fn` do produtor não serviria — ele
  # roda uma vez de qualquer jeito; quem conta leitura é o lado que lê.
  val <- tr_type("d/v", label = "Valor",
                 store = function(x, path) saveRDS(x, path),
                 restore = function(path) { e$restores <- e$restores + 1L; readRDS(path) })

  tr_collection(
    id = "d", version = "1.0.0", label = "Driver de fluxo",
    types = list(val, tr_type("d/w", label = "Outro valor")),
    # Adaptador de aresta: dentro da região ele roda POR PASSO, porque o valor
    # que ele converte é o ponto, que é outro a cada passo.
    adapters = list(tr_adapter("d/v", "d/w", function(x) x * 10),
                    tr_adapter("d/w", "d/v", function(x) x)),
    nodes = list(
      # A FONTE do contrato: o `fn` devolve a lista finita de pontos e o driver
      # caminha nela. `n = 0` é região de zero pontos, que é legítima.
      tr_node("d/fonte", fn = function(n) as.list(seq_len(n)),
              outputs = list(out = ponto()),
              params = list(n = tr_param_int(5L)),
              description = "Emite n pontos: 1..n."),
      # Fonte que devolve o que NÃO é lista: o contrato tem que doer alto, aqui,
      # nomeando a fonte.
      tr_node("d/fonte_torta", fn = function() 42,
              outputs = list(out = ponto()),
              description = "Devolve um escalar onde o driver espera a lista de pontos."),
      # Espelha `data/to_stream`: os pontos vêm de uma tabela ligada por
      # entrada COMUM, e com a entrada solta o `fn` devolve o próprio default.
      tr_node("d/fonte_ext", fn = function(dados = NULL) dados,
              inputs = list(dados = tr_port("d/v", required = FALSE)),
              outputs = list(out = ponto()),
              description = "Parte em pontos o que vem de fora da região."),
      tr_node("d/const", fn = function(v) v, outputs = list(out = "d/v"),
              params = list(v = tr_param_num(100)),
              description = "Valor comum, produzido fora da região."),
      tr_node("d/puro", fn = function(x) {
                e$puros <- c(e$puros, list(x)); x * 2
              },
              inputs = list(x = "d/v"), outputs = list(out = "d/v"),
              description = "Nó comum, elevado ponto a ponto."),
      # Duas entradas: uma que vem de dentro (ponto) e uma comum, de fora. É o
      # "constante vinda de fora da região" do desenho.
      tr_node("d/puro_comum", fn = function(x, k) {
                e$comuns <- c(e$comuns, list(k)); x + k
              },
              inputs = list(x = "d/v", k = "d/v"), outputs = list(out = "d/v"),
              description = "Ponto mais a constante de fora."),
      # Média corrente: o nó COM MEMÓRIA mínimo que prova o encadeamento do
      # estado. Se `state` não atravessar o passo, `n` fica em 1 e a média vira
      # o último ponto — plausível e errado.
      tr_node("d/media", fn = function(x, k) x,
              inputs = list(x = ponto(), k = tr_port("d/v", required = FALSE)),
              outputs = list(out = ponto()),
              params = list(peso = tr_param_num(1)),
              init = function(peso) {
                e$inits <- c(e$inits, list(list(peso = peso)))
                list(soma = 0, n = 0L, peso = peso)
              },
              step = function(state, x, k = NA_real_) {
                e$steps <- c(e$steps, list(list(state = state, x = x, k = k)))
                state$soma <- state$soma + x * state$peso
                state$n <- state$n + 1L
                list(state = state, out = state$soma / state$n)
              },
              description = "Média corrente dos pontos."),
      tr_node("d/media_torta", fn = function(x) x,
              inputs = list(x = ponto()), outputs = list(out = ponto()),
              init = function() list(i = 0L),
              step = function(state, x) {
                state$i <- state$i + 1L
                if (state$i == 3L) return(x)   # devolve o ponto, não list(state=, out=)
                list(state = state, out = x)
              },
              description = "Devolve a forma errada no terceiro passo."),
      tr_node("d/media_explode", fn = function(x) x,
              inputs = list(x = ponto()), outputs = list(out = ponto()),
              init = function() list(i = 0L),
              step = function(state, x) {
                state$i <- state$i + 1L
                if (state$i == 3L) rlang::abort("cavalo desembestado", class = "tr_error_teste")
                list(state = state, out = x)
              },
              description = "Explode no terceiro passo."),
      # O COLAPSO: recebe o fluxo INTEIRO, uma vez, e é ele quem monta o
      # histórico — inclusive o vazio. O núcleo não sabe montar "histórico de
      # zero pontos" de um tipo que ele não conhece.
      tr_node("d/colapsa", fn = function(x) {
                e$colapsos <- c(e$colapsos, list(x))
                data.frame(v = if (length(x)) vapply(x, as.numeric, 0) else numeric())
              },
              inputs = list(x = ponto()), outputs = list(out = "d/v"),
              description = "Monta o histórico do fluxo."),
      # Colapso com entrada comum além do fluxo: a constante não é acumulada.
      tr_node("d/colapsa_comum", fn = function(x, k) {
                e$colapsos <- c(e$colapsos, list(list(x = x, k = k)))
                data.frame(v = if (length(x)) vapply(x, as.numeric, 0) else numeric(), k = k)
              },
              inputs = list(x = ponto(), k = "d/v"), outputs = list(out = "d/v"),
              description = "Histórico com uma constante de fora."),
      tr_node("d/colapsa_explode", fn = function(x) rlang::abort("não juntou",
                                                                class = "tr_error_teste"),
              inputs = list(x = ponto()), outputs = list(out = "d/v"),
              description = "Não consegue montar o histórico."),
      tr_node("d/colapsa_terminal", fn = function(x) {
                e$colapsos <- c(e$colapsos, list(x)); invisible(NULL)
              },
              inputs = list(x = ponto()),
              description = "Colapso sem porta de saída."),
      # Nó elevado de OUTRO tipo: a aresta que o alimenta ganha adaptador.
      tr_node("d/puro_w", fn = function(x) {
                e$puros <- c(e$puros, list(x)); x
              },
              inputs = list(x = "d/w"), outputs = list(out = "d/w"),
              description = "Elevado, e de outro tipo que o da fonte."),
      # Porta VARIÁDICA, e a ordem dela importa: `a - b` denuncia troca de
      # posição, o que uma soma esconderia.
      tr_node("d/menos", fn = function(xs) {
                v <- unlist(xs); v[[1]] - v[[2]]
              },
              inputs = list(xs = tr_port("d/v", multiple = TRUE)),
              outputs = list(out = "d/v"),
              description = "Primeira fonte menos a segunda, na ordem do índice."),
      tr_node("d/menos3", fn = function(xs) {
                v <- unlist(xs); v[[1]] - v[[2]] - v[[3]]
              },
              inputs = list(xs = tr_port("d/v", multiple = TRUE)),
              outputs = list(out = "d/v"),
              description = "Três fontes variádicas, na ordem do índice."),
      tr_node("d/mostra", fn = function(x) invisible(x), inputs = list(x = "d/v"),
              description = "Consome o histórico, fora da região.")
    )
  )
}

driver_registry <- function(e = diario()) {
  reg <- tr_registry(); tr_use(driver_collection(e), registry = reg); reg
}

# Roda a região do documento como o worker roda: uma unidade, `.tr_run_unit()`.
# Direto, e não por `tr_run()`, porque o que está sob teste é o driver — pelo
# scheduler um erro vira evento e a mensagem chega embrulhada.
roda_regiao <- function(doc, reg, store, colapso = "co") {
  p <- tr_plan(doc, registry = reg, store = store)
  u <- p$units[[colapso]]
  expect_equal(u$kind, "stream_region")
  list(handles = .tr_run_unit(u, reg, store), unit = u)
}

test_that("média corrente de 5 pontos: histórico com 5 linhas e a última média é mean(x)", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 5L) |>
    tr_add("ac", "d/media", from = "fo") |>
    tr_add("co", "d/colapsa", from = "ac"))

  r <- roda_regiao(doc, reg, s)
  hist <- tr_store_get(s, r$unit$outputs$out, tr_get_type("d/v", reg))
  expect_equal(nrow(hist), 5L)
  expect_equal(hist$v[[5]], mean(1:5))
  # E a curva inteira é a média corrente, não cinco vezes o mesmo valor.
  expect_equal(hist$v, cumsum(1:5) / seq_len(5))
})

test_that("nó puro elevado é chamado uma vez por ponto, com o ponto do passo", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 4L) |>
    tr_add("pu", "d/puro", from = "fo") |>
    tr_add("co", "d/colapsa", from = "pu"))

  r <- roda_regiao(doc, reg, s)
  # Quatro chamadas, uma por ponto, na ordem — e não uma chamada com a lista.
  expect_equal(length(e$puros), 4L)
  expect_equal(unlist(e$puros), 1:4)
  hist <- tr_store_get(s, r$unit$outputs$out, tr_get_type("d/v", reg))
  expect_equal(hist$v, c(2, 4, 6, 8))
})

test_that("`init` roda uma vez e recebe os params EFETIVOS do nó", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("ac", "d/media", peso = 2, from = "fo") |>
    tr_add("co", "d/colapsa", from = "ac"))

  r <- roda_regiao(doc, reg, s)
  expect_equal(length(e$inits), 1L)
  expect_equal(e$inits[[1]]$peso, 2)
  # O param chegou ao estado e a média corrente saiu dobrada: `init` rodando
  # com o default (1) daria a média simples, e nada erraria alto.
  hist <- tr_store_get(s, r$unit$outputs$out, tr_get_type("d/v", reg))
  expect_equal(hist$v, 2 * cumsum(1:3) / seq_len(3))
})

test_that("`step` recebe o `state` do passo anterior e o ponto do passo", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("ac", "d/media", from = "fo") |>
    tr_add("co", "d/colapsa", from = "ac"))

  roda_regiao(doc, reg, s)
  expect_equal(length(e$steps), 3L)
  expect_equal(vapply(e$steps, function(p) p$x, 0), c(1, 2, 3))
  # O estado ENTRA em cada passo como saiu do anterior: 0, 1, 2 contagens.
  expect_equal(vapply(e$steps, function(p) p$state$n, 0L), c(0L, 1L, 2L))
  expect_equal(vapply(e$steps, function(p) p$state$soma, 0), c(0, 1, 3))
})

test_that("entrada comum chega igual em todos os passos e é lida do store UMA vez", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("k", "d/const", v = 7) |>
    tr_add("fo", "d/fonte", n = 5L) |>
    tr_add("ac", "d/media", from = "fo") |>
    tr_link("k:out", "ac:k") |>
    tr_add("co", "d/colapsa", from = "ac"))

  # `k` é nó comum: roda antes, por fora da região.
  p <- tr_plan(doc, registry = reg, store = s)
  .tr_run_unit(p$units$k, reg, s)
  lidas_antes <- e$restores

  u <- p$units$co
  expect_equal(u$kind, "stream_region")
  .tr_run_unit(u, reg, s)

  # Cinco passos, o mesmo valor em todos.
  expect_equal(vapply(e$steps, function(p) p$k, 0), rep(7, 5))
  # E UMA leitura do store para os cinco passos. Ler por passo seriam N mil
  # leituras do mesmo artefato — e o artefato comum é o mais pesado do fluxo.
  expect_equal(e$restores - lidas_antes, 1L)
})

test_that("erro dentro de `step` sobe como erro da unidade, nomeando o nó e o passo", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 5L) |>
    tr_add("ac", "d/media_explode", from = "fo") |>
    tr_add("co", "d/colapsa", from = "ac"))
  u <- tr_plan(doc, registry = reg, store = s)$units$co

  err <- expect_error(.tr_run_unit(u, reg, s), class = "tr_error_stream_step")
  msg <- conditionMessage(err)
  expect_match(msg, "'ac'")
  expect_match(msg, "passo 3")
  # A causa original tem que sobreviver: sem ela o autor do nó recebe "a região
  # falhou" e nada sobre o quê.
  expect_match(msg, "cavalo desembestado")
})

test_that("`step` que devolve a forma errada erra alto, com o nó e o passo", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 5L) |>
    tr_add("ac", "d/media_torta", from = "fo") |>
    tr_add("co", "d/colapsa", from = "ac"))
  u <- tr_plan(doc, registry = reg, store = s)$units$co

  err <- expect_error(.tr_run_unit(u, reg, s), class = "tr_error_bad_step")
  expect_match(conditionMessage(err), "'ac'")
  expect_match(conditionMessage(err), "passo 3")
})

test_that("fonte que não devolve a lista de pontos erra alto, nomeando a fonte", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte_torta") |>
    tr_add("co", "d/colapsa", from = "fo"))
  u <- tr_plan(doc, registry = reg, store = s)$units$co

  err <- expect_error(.tr_run_unit(u, reg, s), class = "tr_error_stream_bad_source")
  expect_match(conditionMessage(err), "'fo'")
})

test_that("o colapso recebe o fluxo INTEIRO, numa chamada só", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 4L) |>
    tr_add("co", "d/colapsa", from = "fo"))

  roda_regiao(doc, reg, s)
  # UMA chamada — o colapso não é elevado ponto a ponto. É o contrato que a
  # Fase 6 implementa em `data/from_stream`: é o colapso, e não o driver, que
  # sabe juntar N pontos de um tipo no histórico desse tipo.
  expect_equal(length(e$colapsos), 1L)
  expect_equal(unlist(e$colapsos[[1]]), 1:4)
})

test_that("no colapso, a entrada comum NÃO é acumulada: é constante", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("k", "d/const", v = 9) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("co", "d/colapsa_comum", from = "fo") |>
    tr_link("k:out", "co:k"))

  p <- tr_plan(doc, registry = reg, store = s)
  .tr_run_unit(p$units$k, reg, s)
  .tr_run_unit(p$units$co, reg, s)

  expect_equal(length(e$colapsos), 1L)
  expect_equal(unlist(e$colapsos[[1]]$x), 1:3)
  expect_equal(e$colapsos[[1]]$k, 9)
})

test_that("região de ZERO pontos produz histórico vazio, não erro", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 0L) |>
    tr_add("ac", "d/media", from = "fo") |>
    tr_add("co", "d/colapsa", from = "ac"))

  r <- roda_regiao(doc, reg, s)
  # `init` rodou (o estado inicial existe), `step` nunca, e o colapso foi
  # chamado uma vez com a lista vazia: quem decide o que é "histórico vazio"
  # deste tipo é o colapso, não o núcleo.
  expect_equal(length(e$inits), 1L)
  expect_equal(length(e$steps), 0L)
  expect_equal(length(e$colapsos), 1L)
  expect_equal(length(e$colapsos[[1]]), 0L)
  hist <- tr_store_get(s, r$unit$outputs$out, tr_get_type("d/v", reg))
  expect_equal(nrow(hist), 0L)
})

test_that("fonte com a entrada solta devolve NULL, e NULL é zero pontos", {
  # `data/to_stream` sem tabela ligada devolve o próprio default. Recusar isso
  # quebraria um grafo que a validação da região ACEITA (a porta é opcional), e
  # a região sairia `failed` gravando um erro sob a chave dela — que nenhum
  # `tr_plan()` recalcula depois.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte_ext") |>
    tr_add("co", "d/colapsa", from = "fo"))

  r <- roda_regiao(doc, reg, s)
  expect_equal(length(e$colapsos), 1L)
  expect_equal(length(e$colapsos[[1]]), 0L)
  hist <- tr_store_get(s, r$unit$outputs$out, tr_get_type("d/v", reg))
  expect_equal(nrow(hist), 0L)
})

test_that("fonte que devolve os pontos vindos de FORA da região caminha neles", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("k", "d/const", v = 4) |>
    tr_add("fo", "d/fonte_ext", from = "k") |>
    tr_add("pu", "d/puro", from = "fo") |>
    tr_add("co", "d/colapsa", from = "pu"))

  # `d/const` devolve um escalar; para virar fonte ele tem que virar lista, e é
  # a fonte que fatia. Aqui o escalar chega cru: o contrato erra alto.
  u <- tr_plan(doc, registry = reg, store = s)$units$co
  p <- tr_plan(doc, registry = reg, store = s)
  .tr_run_unit(p$units$k, reg, s)
  err <- expect_error(.tr_run_unit(u, reg, s), class = "tr_error_stream_bad_source")
  expect_match(conditionMessage(err), "'fo'")
})

test_that("colapso sem porta de saída grava o marcador, como todo nó terminal", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 2L) |>
    tr_add("co", "d/colapsa_terminal", from = "fo"))

  r <- roda_regiao(doc, reg, s)
  expect_equal(length(r$handles), 1L)
  h <- tr_store_handle(s, r$unit$outputs[[1]])
  expect_false(is.null(h))
  expect_equal(h$type, "trama/marker")
})

test_that("cada colapso da MESMA região deixa o próprio histórico na própria saída", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  # fo -> ac -> c1 (média corrente) ; fo -> c2 (os pontos crus). Uma execução,
  # duas saídas: emitir uma unidade por colapso faria a região rodar duas vezes.
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("ac", "d/media", from = "fo") |>
    tr_add("c1", "d/colapsa", from = "ac") |>
    tr_add("c2", "d/colapsa") |>
    tr_link("fo:out", "c2:x"))

  r <- roda_regiao(doc, reg, s, colapso = "c1")
  # A fonte rodou UMA vez para os dois colapsos: dois `init` seriam duas
  # execuções da região disfarçadas.
  expect_equal(length(e$inits), 1L)
  expect_equal(length(e$steps), 3L)

  ty <- tr_get_type("d/v", reg)
  h1 <- tr_store_get(s, r$unit$outputs[["c1:out"]], ty)
  h2 <- tr_store_get(s, r$unit$outputs[["c2:out"]], ty)
  expect_equal(h1$v, cumsum(1:3) / seq_len(3))
  expect_equal(h2$v, c(1, 2, 3))
})

test_that("a região executa no run completo e o jusante consome o histórico", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("ac", "d/media", from = "fo") |>
    tr_add("co", "d/colapsa", from = "ac") |>
    tr_add("mo", "d/mostra", from = "co"))

  ev <- list()
  r <- tr_run(doc, registry = reg, store = s, on_event = function(x) ev[[length(ev) + 1]] <<- x)
  # `done` de uma região sai pelos COLAPSOS: são os únicos ids dela que o resto
  # do grafo consome.
  expect_setequal(r$done, c("co", "mo"))
  expect_length(r$skipped, 0L)
  # Segunda rodada: a região é UMA unidade, UMA chave — vem do cache inteira, e
  # nenhum `init` roda de novo.
  n_inits <- length(e$inits)
  r2 <- tr_run(doc, registry = reg, store = s)
  expect_length(r2$skipped, 0L)
  expect_equal(length(e$inits), n_inits)
})

test_that("entrada comum de FORA do grafo da região atravessa o run inteiro", {
  # O caminho completo: nó comum -> região (entrada `external`) -> colapso ->
  # nó comum. É o `unit$inputs` da região sendo lido pelo mesmo `.tr_load_ref()`
  # de um nó qualquer, adaptador incluído.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("k", "d/const", v = 3) |>
    tr_add("fo", "d/fonte", n = 2L) |>
    tr_add("pu", "d/puro_comum", from = "fo") |>
    tr_link("k:out", "pu:k") |>
    tr_add("co", "d/colapsa", from = "pu") |>
    tr_add("mo", "d/mostra", from = "co"))

  r <- tr_run(doc, registry = reg, store = s)
  expect_setequal(r$done, c("k", "co", "mo"))
  expect_equal(unlist(e$comuns), c(3, 3))
  u <- tr_plan(doc, registry = reg, store = s)$units$co
  hist <- tr_store_get(s, u$outputs$out, tr_get_type("d/v", reg))
  expect_equal(hist$v, c(4, 5))
})

test_that("adaptador de aresta INTERNA roda por passo, no ponto do passo", {
  # O adaptador converte o PONTO, não o fluxo: rodá-lo uma vez (no que a fonte
  # devolveu) converteria a lista de pontos, e o nó elevado receberia a lista.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("pw", "d/puro_w", from = "fo") |>
    tr_add("co", "d/colapsa", from = "pw"))

  r <- roda_regiao(doc, reg, s)
  expect_equal(unlist(e$puros), c(10, 20, 30))
  hist <- tr_store_get(s, r$unit$outputs$out, tr_get_type("d/v", reg))
  expect_equal(hist$v, c(10, 20, 30))
})

test_that("porta variádica meio de dentro, meio de fora, na ordem do índice", {
  # `pos` é a posição da fonte EXTERNA entre as externas da mesma porta, e é
  # com ela que o driver remonta a ordem do `index`. Trocar as duas daria um
  # histórico plausível e de sinal invertido.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("k", "d/const", v = 100) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("me", "d/menos") |>
    tr_link("fo:out", "me:xs", index = 1L) |>
    tr_link("k:out", "me:xs", index = 2L) |>
    tr_add("co", "d/colapsa", from = "me"))

  p <- tr_plan(doc, registry = reg, store = s)
  .tr_run_unit(p$units$k, reg, s)
  .tr_run_unit(p$units$co, reg, s)
  hist <- tr_store_get(s, p$units$co$outputs$out, tr_get_type("d/v", reg))
  expect_equal(hist$v, c(1, 2, 3) - 100)

  # E com os índices trocados o sinal inverte: a ordem é lida, não presumida.
  e2 <- diario(); reg2 <- driver_registry(e2); s2 <- tmp_store()
  doc2 <- tr_flow_doc(tr_flow(reg2) |>
    tr_add("k", "d/const", v = 100) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("me", "d/menos") |>
    tr_link("k:out", "me:xs", index = 1L) |>
    tr_link("fo:out", "me:xs", index = 2L) |>
    tr_add("co", "d/colapsa", from = "me"))
  p2 <- tr_plan(doc2, registry = reg2, store = s2)
  .tr_run_unit(p2$units$k, reg2, s2)
  .tr_run_unit(p2$units$co, reg2, s2)
  hist2 <- tr_store_get(s2, p2$units$co$outputs$out, tr_get_type("d/v", reg2))
  expect_equal(hist2$v, 100 - c(1, 2, 3))
})

test_that("porta variádica com DUAS fontes externas: `pos` indexa dentro da porta", {
  # Com uma externa só, `pos` é sempre 1 e qualquer erro nele passa. Duas
  # externas em índices NÃO contíguos (1 e 3, com o ponto no 2) é o menor grafo
  # em que trocar `pos` troca os valores — e trocá-los dá um histórico
  # plausível, do sinal errado.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("k1", "d/const", v = 100) |>
    tr_add("k2", "d/const", v = 5) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("me", "d/menos3") |>
    tr_link("k1:out", "me:xs", index = 1L) |>
    tr_link("fo:out", "me:xs", index = 2L) |>
    tr_link("k2:out", "me:xs", index = 3L) |>
    tr_add("co", "d/colapsa", from = "me"))

  p <- tr_plan(doc, registry = reg, store = s)
  .tr_run_unit(p$units$k1, reg, s); .tr_run_unit(p$units$k2, reg, s)
  .tr_run_unit(p$units$co, reg, s)
  hist <- tr_store_get(s, p$units$co$outputs$out, tr_get_type("d/v", reg))
  expect_equal(hist$v, 100 - c(1, 2, 3) - 5)
})

test_that("os handles do `done` são nomeados como as saídas da REGIÃO", {
  # O caminho `cached` do plano entrega `u$handles` nomeado por
  # "<colapso>:<porta>"; o `done` tem que entregar igual. Com o nome de porta
  # cru, dois colapsos dariam duas chaves "out" no mesmo evento — e o
  # `handles.out || primeiro valor` do front leria o histórico do outro colapso.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 2L) |>
    tr_add("ac", "d/media", from = "fo") |>
    tr_add("c1", "d/colapsa", from = "ac") |>
    tr_add("c2", "d/colapsa") |>
    tr_link("fo:out", "c2:x"))

  r <- roda_regiao(doc, reg, s, colapso = "c1")
  expect_setequal(names(r$handles), names(r$unit$outputs))

  # E é o mesmo conjunto de nomes que o plano seguinte entrega pelo cache.
  p2 <- tr_plan(doc, registry = reg, store = s)
  expect_true(p2$units$c1$cached)
  expect_setequal(names(p2$units$c1$handles), names(r$handles))
})

test_that("erro no colapso nomeia o COLAPSO, não a região inteira", {
  # O evento `failed` da região sai sob `u$node`, que é o PRIMEIRO colapso: numa
  # região de dois, a falha do segundo apareceria no card do primeiro sem uma
  # palavra sobre de quem foi.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 2L) |>
    tr_add("c1", "d/colapsa", from = "fo") |>
    tr_add("c2", "d/colapsa_explode") |>
    tr_link("fo:out", "c2:x"))
  u <- tr_plan(doc, registry = reg, store = s)$units$c1

  err <- expect_error(.tr_run_unit(u, reg, s), class = "tr_error_stream_collapse")
  expect_match(conditionMessage(err), "'c2'")
  expect_match(conditionMessage(err), "não juntou")
})

# --- Ponta a ponta: a região pelo scheduler, não pelo worker ----------------

test_that("tr_run grava o histórico da região, e a segunda vez vem do cache", {
  # Os testes acima chamam `.tr_run_unit()` direto, porque o que estava sob
  # teste era o driver. Este é o único que passa pelo caminho de verdade —
  # plano, scheduler, executor, store — e é ele que prova a fase.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 6L) |>
    tr_add("ac", "d/media", from = "fo") |>
    tr_add("co", "d/colapsa", from = "ac") |>
    tr_add("ver", "d/mostra", from = "co"))

  ev1 <- list()
  tr_run(doc, registry = reg, store = s,
         on_event = function(x) ev1[[length(ev1) + 1]] <<- x)

  # 1. O artefato existe, é o histórico, e tem uma linha por ponto.
  u <- tr_plan(doc, registry = reg, store = s)$units[["co"]]
  hist <- tr_store_get(s, u$outputs$out, tr_get_type("d/v", reg))
  expect_equal(nrow(hist), 6L)
  expect_equal(hist$v, cumsum(1:6) / seq_len(6))

  # 2. A região rodou uma vez, com um `step` por ponto, e o consumidor a
  #    jusante do colapso rodou depois dela — grafo comum, como sempre.
  tipos <- function(ev, no) vapply(Filter(function(x) identical(x$node, no), ev),
                                   function(x) x$type, "")
  expect_equal(tipos(ev1, "co"), c("running", "done"))
  expect_true("done" %in% tipos(ev1, "ver"))
  expect_equal(length(e$steps), 6L)

  # 3. Rodar de novo NÃO roda o driver: a chave é a mesma, o artefato está lá,
  #    e o plano decide o cache hit sem acordar worker nenhum. O contador de
  #    `step` é a prova que um `expect_equal` de handle não daria — handle
  #    igual seria igual mesmo se tudo tivesse recomputado.
  p2 <- tr_plan(doc, registry = reg, store = s)
  expect_true(isTRUE(p2$units[["co"]]$cached))
  expect_length(tr_plan_pending(p2), 0L)

  ev2 <- list()
  tr_run(doc, registry = reg, store = s,
         on_event = function(x) ev2[[length(ev2) + 1]] <<- x)
  expect_equal(tipos(ev2, "co"), "cached")
  expect_equal(length(e$steps), 6L)   # continua 6: nenhum passo novo
})
