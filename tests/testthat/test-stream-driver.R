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
  e$sono <- 0            # segundos que cada leitura de artefato demora
  e$morre_em <- NULL     # ponto em que `d/puro_morre` explode (NULL = nunca)
  e$seeds <- list()      # o `.seed` de cada chamada de `fn` elevado
  e$seeds_step <- list() # o `.seed` de cada chamada de `step`
  e$seed_fonte <- NULL   # o `.seed` da fonte, que roda uma vez
  e$faltou <- logical()  # `.seed` chegou AUSENTE nesta chamada?
  e
}

driver_collection <- function(e = diario()) {
  ponto <- function(...) tr_port("d/v", stream = TRUE, ...)

  # `restore` que conta: é a prova de que a entrada comum é lida UMA vez, e não
  # uma por passo. Um contador dentro do `fn` do produtor não serviria — ele
  # roda uma vez de qualquer jeito; quem conta leitura é o lado que lê.
  #
  # E `e$sono` faz a leitura DEMORAR, o que é a única forma de medir que o
  # `duration` da região não inclui as leituras: com o cronômetro no lugar
  # errado a região sai com o resultado certo e o tempo do arquivo pesado.
  val <- tr_type("d/v", label = "Valor",
                 store = function(x, path) saveRDS(x, path),
                 restore = function(path) {
                   e$restores <- e$restores + 1L
                   if (e$sono > 0) Sys.sleep(e$sono)
                   readRDS(path)
                 })

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
      # A fonte que ESQUECEU DE FATIAR: devolve a tabela inteira. É list-like,
      # então a guarda ingênua (`!is.list()`) a deixa passar e o driver anda nas
      # COLUNAS — um fluxo de três pontos, cada ponto um vetor de cem. Nada erra:
      # o histórico sai com três linhas plausíveis. É o erro mais provável de
      # quem escreve uma fonte, e o único do arquivo que termina em silêncio.
      tr_node("d/fonte_tabela", fn = function() data.frame(a = 1:100, b = 101:200, c = 201:300),
              outputs = list(out = ponto()),
              description = "Devolve a tabela inteira, sem fatiar em pontos."),
      # Fatiou CERTO, e a lista de pontos tem classe própria — é a forma do
      # `dplyr::group_split()`, que devolve `vctrs_list_of`. Tem que passar.
      tr_node("d/fonte_classe", fn = function(n = 3L) structure(as.list(seq_len(n)),
                                                                class = "vctrs_list_of"),
              params = list(n = tr_param_num(3)),
              outputs = list(out = ponto()),
              description = "Fatia em pontos, devolvendo lista com classe própria."),
      # Espelha `s/fonte_dupla` e a forma real de `data/to_stream` com resumo:
      # DUAS saídas e a entrada opcional solta, então o `fn` devolve NULL. É a
      # fonte em que "NULL vale como zero pontos" tem que continuar valendo.
      tr_node("d/fonte_dupla", fn = function(dados = NULL) dados,
              inputs = list(dados = tr_port("d/v", required = FALSE)),
              outputs = list(fluxo = ponto(), resumo = "d/v"),
              description = "Pontos por uma saída, resumo comum pela outra."),
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
      # Morre num ponto ESCOLHIDO, e a escolha vem do diário — não de um param.
      # É essa diferença que faz o teste de retomada medir o que promete: o run
      # que morre e o run que retoma têm que ter a MESMA chave de região, senão
      # o checkpoint não é achado. Um param `em` entraria na chave (e é certo
      # que entre — é conteúdo do grafo); `e$morre_em` é estado do processo de
      # teste, e o corpo do `fn` é o mesmo nos dois runs.
      tr_node("d/puro_morre", fn = function(x) {
                if (!is.null(e$morre_em) && isTRUE(as.numeric(x) == e$morre_em)) {
                  rlang::abort("worker morto", class = "tr_error_teste")
                }
                e$puros <- c(e$puros, list(x)); x
              },
              inputs = list(x = "d/v"), outputs = list(out = "d/v"),
              description = "Passa o ponto adiante, e morre no ponto escolhido."),
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
      # `k = NULL` no formal do `fn`: `k` é porta OPCIONAL, e porta opcional
      # solta não é passada — sem o default o nó aborta "argumento ausente, sem
      # padrão" assim que o corpo tocar `k`. O `step` já traz `k = NA_real_` pela
      # mesma razão; era só o `fn` que faltava.
      tr_node("d/media", fn = function(x, k = NULL) x,
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
      # --- Os nós de `.seed` ------------------------------------------------
      #
      # O sorteio é a SAÍDA do nó, e não `x + runif(1)`: com o ponto somado, um
      # histórico de valores diferentes sairia igual mesmo se o sorteio fosse o
      # mesmo nos n passos (1+u, 2+u, 3+u), e o teste de "sorteia diferente em
      # cada passo" passaria sem medir nada.
      tr_node("d/sorteia", fn = function(x, .seed) {
                set.seed(.seed)
                v <- stats::runif(1)
                e$seeds <- c(e$seeds, list(.seed))
                v
              },
              inputs = list(x = "d/v"), outputs = list(out = "d/v"),
              stochastic = TRUE,
              description = "Sorteia um número a partir do .seed do passo."),
      # Declara `.seed` e NUNCA o força — o corpo só sorteia. Era o caso CALADO:
      # a avaliação preguiçosa do R esconde o argumento ausente, o nó roda com o
      # RNG global do daemon e o histórico sai plausível sob uma chave que
      # promete depender da seed. `missing()` é o que distingue os dois mundos
      # sem forçar: ele responde "o chamador passou?", que é exatamente a
      # pergunta que ninguém estava fazendo.
      tr_node("d/sorteia_frouxo", fn = function(x, .seed) {
                e$faltou <- c(e$faltou, missing(.seed))
                stats::runif(1)
              },
              inputs = list(x = "d/v"), outputs = list(out = "d/v"),
              stochastic = TRUE,
              description = "Declara .seed e nunca o força."),
      # Nó COM MEMÓRIA cujo `step` declara `.seed` (`node.R` já permitia). A
      # soma corrente dos sorteios é o que faz a retomada morder: se a seed do
      # passo não fosse pura, o histórico retomado divergiria do inteiro.
      tr_node("d/soma_sorteios", fn = function(x) x,
              inputs = list(x = ponto()), outputs = list(out = ponto()),
              init = function() list(soma = 0),
              step = function(state, x, .seed) {
                set.seed(.seed)
                e$seeds_step <- c(e$seeds_step, list(.seed))
                state$soma <- state$soma + stats::runif(1)
                list(state = state, out = state$soma)
              },
              stochastic = TRUE,
              description = "Soma corrente de sorteios, um por passo."),
      # A FONTE que declara `.seed`: roda UMA vez, antes do laço, e por isso
      # recebe a própria seed do documento — não uma derivada. Nó nenhum pode se
      # comportar diferente dentro e fora de uma região.
      tr_node("d/fonte_seed", fn = function(n, .seed) {
                e$seed_fonte <- .seed
                as.list(seq_len(n))
              },
              outputs = list(out = ponto()),
              params = list(n = tr_param_int(3L)),
              stochastic = TRUE,
              description = "Emite n pontos e registra o .seed que recebeu."),
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

test_that("fonte que devolve a TABELA inteira erra alto, em vez de andar nas colunas", {
  # O caso que a guarda de `!is.list()` sozinha não pega: `data.frame` É uma
  # lista, então a tabela não fatiada passava e o driver andava nela como lista
  # de COLUNAS. Três colunas viravam três pontos, o `fn` elevado recebia o vetor
  # inteiro de cada coluna, e a região TERMINAVA — histórico de três linhas
  # plausível, gravado no store e servido do cache pra sempre. É o esquecimento
  # mais provável de quem escreve uma fonte (`data/to_stream`, Fase 6).
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte_tabela") |>
    tr_add("pu", "d/puro", from = "fo") |>
    tr_add("co", "d/colapsa", from = "pu"))
  u <- tr_plan(doc, registry = reg, store = s)$units$co

  err <- expect_error(.tr_run_unit(u, reg, s), class = "tr_error_stream_bad_source")
  expect_match(conditionMessage(err), "'fo'")
  # A CLASSE do que veio entra na mensagem: sem ela o autor da fonte lê "espera
  # uma lista", olha o próprio `data.frame` (que é uma lista) e não vê o erro.
  expect_match(conditionMessage(err), "data.frame")
  expect_match(conditionMessage(err), "quem fatia é a fonte")
  # E nada rodou: nenhuma chamada do nó elevado, nenhum colapso.
  expect_equal(length(e$puros), 0L)
  expect_equal(length(e$colapsos), 0L)
})

test_that("a guarda da fonte pergunta se é RETÂNGULO, não se tem classe", {
  # `dim()`, e não `is.object()`: pega `data.frame`/`tbl_df`/`sf`/`data.table`
  # sem o núcleo conhecer tipo de domínio nenhum, e deixa passar lista de pontos
  # COM classe própria. Essa última linha é a que importa: `dplyr::group_split()`
  # devolve `vctrs_list_of`, e é a forma mais idiomática de fatiar uma tabela por
  # coluna — o que `data/to_stream(por = )` vai fazer na Fase 6. Com
  # `is.object()` a fonte fatiava certo e era recusada.
  retangulo <- function(v) !is.list(v) || !is.null(dim(v))

  expect_false(retangulo(list(1, 2)))
  expect_false(retangulo(structure(list(1, 2), class = "vctrs_list_of")))
  expect_false(retangulo(list(data.frame(a = 1), data.frame(a = 2))))
  expect_true(retangulo(data.frame(a = 1)))
  expect_true(retangulo(42))
  expect_true(retangulo(matrix(1:4, 2)))
})

test_that("fonte que fatiou com classe própria (group_split) é ACEITA", {
  # A prova de ponta a ponta do parágrafo acima: a lista de pontos tem classe,
  # e a região roda.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte_classe", n = 3L) |>
    tr_add("pu", "d/puro", from = "fo") |>
    tr_add("co", "d/colapsa", from = "pu"))

  roda_regiao(doc, reg, s)
  expect_equal(length(e$puros), 3L)           # um por ponto, não um pela lista
  expect_equal(unlist(e$puros), 1:3)          # e cada um com o ponto da vez
  expect_equal(unlist(e$colapsos[[1]]), c(2, 4, 6))   # `d/puro` dobra
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

test_that("NULL é zero pontos também na fonte de VÁRIAS saídas", {
  # `d/fonte_dupla` é a forma de `data/to_stream` com resumo: duas saídas, e com
  # a tabela desligada o `fn` devolve o próprio default, NULL. A checagem de
  # "devolveu todas as portas declaradas" rodava ANTES da normalização de NULL e
  # a região morria com `tr_error_bad_output` — "declara as saídas fluxo, resumo
  # mas devolveu NULL" —, num grafo que a validação da região ACEITA (a porta é
  # opcional). E o erro ia pra chave da região, que nenhum `tr_plan()` recalcula.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte_dupla") |>
    tr_add("co", "d/colapsa") |>
    tr_link("fo:fluxo", "co:x"))

  r <- roda_regiao(doc, reg, s)
  # Zero pontos, o colapso chamado UMA vez com a lista vazia, histórico vazio —
  # o mesmo comportamento da fonte de uma saída só.
  expect_equal(length(e$colapsos), 1L)
  expect_equal(e$colapsos[[1]], list())
  expect_equal(length(e$puros), 0L)
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

test_that("o `duration` da região NÃO inclui a leitura dos artefatos de fora", {
  # `duration` é o número que o front mostra no card, e tem que significar nos
  # dois executores a mesma coisa que significa num nó comum: o tempo do
  # trabalho, não o da leitura dos inputs. O que vem de fora da região é
  # justamente o artefato mais pesado do fluxo (o modelo treinado), então com o
  # cronômetro acima das leituras o card diria que o laço demorou o tempo de
  # carregar um arquivo — e nenhum teste de resultado percebe, porque o
  # histórico sai idêntico. `e$sono` faz a leitura demorar pra que a diferença
  # seja mensurável.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("k", "d/const", v = 7) |>
    tr_add("fo", "d/fonte", n = 3L) |>
    tr_add("ac", "d/media", from = "fo") |>
    tr_link("k:out", "ac:k") |>
    tr_add("co", "d/colapsa", from = "ac"))

  p <- tr_plan(doc, registry = reg, store = s)
  .tr_run_unit(p$units$k, reg, s)   # antes do sono: `k` não lê nada

  e$sono <- 0.4
  t0 <- Sys.time()
  hs <- .tr_run_unit(p$units$co, reg, s)
  parede <- as.numeric(Sys.time() - t0, units = "secs")
  e$sono <- 0

  # A leitura lenta ACONTECEU dentro da execução da região — sem isto o teste
  # passaria por não ter medido nada.
  expect_equal(e$restores, 1L)
  expect_gte(parede, 0.4)
  # E o `duration` gravado ficou de fora dela.
  expect_lt(hs$out$duration, 0.3)
  expect_lt(tr_store_handle(s, p$units$co$outputs$out)$duration, 0.3)
})

test_that("nenhum nó das coleções de teste tem porta opcional sem default no formal", {
  # A classe de defeito que já quebrou `s/fonte` e `s/fonte_dupla`: formal que
  # corresponde a porta `required = FALSE` e não tem default. Porta opcional
  # solta não é passada, então o nó aborta "argumento ausente, sem padrão" no
  # instante em que o corpo tocar o argumento — e fica latente até lá, porque um
  # corpo que ignora o argumento nunca o força.
  expect_equal(portas_opcionais_sem_default(driver_collection(diario())), character())
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

# --- Checkpoint e retomada --------------------------------------------------
#
# `R/executor.R` diz, com essas palavras: "Projete o nó para o worker morrer a
# qualquer momento". Cancelar é MATAR o daemon, porque cancelamento cooperativo
# em R não existe, e a revisão da Fase 4 mediu que todo o trabalho em voo se
# perde no kill. A região é a unidade mais longa que o trama vai ter: morrer no
# passo 9.999 de 10.000 não pode custar o run inteiro.
#
# É também o teste que paga a Decisão 8 (estado explícito e serializável, por
# `init`/`step`, em vez de closure). Se o checkpoint não funciona, aquela decisão
# não comprou nada.

# O documento dos testes de retomada: 250 pontos, um nó que pode morrer no ponto
# escolhido, um nó COM MEMÓRIA (média corrente) e o colapso. A média corrente é
# o que faz o `expect_identical` morder: sem `estado` no checkpoint, a média
# recomeça no passo 101 e o histórico sai plausível e errado.
doc_ckpt <- function(reg, n = 250L) {
  tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = n) |>
    tr_add("mo", "d/puro_morre", from = "fo") |>
    tr_add("ac", "d/media", from = "mo") |>
    tr_add("co", "d/colapsa", from = "ac"))
}

ckpt_path <- function(store, key) file.path(store$root, "stream", key, "ckpt.rds")

test_that("run morto no passo 180 retoma do passo 100, e o histórico sai IDÊNTICO", {
  # Este `expect_identical` é a tarefa inteira: retomada que produz resultado
  # diferente é pior que retomada nenhuma — o histórico errado vai pro store sob
  # uma chave válida e é servido do cache pra sempre.
  e <- diario(); reg <- driver_registry(e)
  doc <- doc_ckpt(reg)

  # 1. A referência: sem interrupção nenhuma, em store próprio.
  s0 <- tmp_store()
  u0 <- tr_plan(doc, registry = reg, store = s0)$units$co
  .tr_run_unit(u0, reg, s0, ctx_extra = list(checkpoint_every = 100))
  ref <- tr_store_get(s0, u0$outputs$out, tr_get_type("d/v", reg))
  expect_equal(nrow(ref), 250L)

  # 2. O run que morre no ponto 180.
  s <- tmp_store()
  u <- tr_plan(doc, registry = reg, store = s)$units$co
  # A chave é a MESMA dos dois lados — é ela que faz o checkpoint ser achado, e
  # é por construção que um checkpoint de chave diferente nunca é lido.
  expect_identical(u$key, u0$key)
  e$morre_em <- 180
  expect_error(.tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 100)),
               class = "tr_error_stream_step")

  # O checkpoint sobreviveu à falha, no último múltiplo de 100 antes de 180.
  expect_true(file.exists(ckpt_path(s, u$key)))
  expect_equal(readRDS(ckpt_path(s, u$key))$i, 100L)

  # 3. A retomada, com a mesma chave.
  e$morre_em <- NULL
  antes <- length(e$puros)
  .tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 100))
  hist <- tr_store_get(s, u$outputs$out, tr_get_type("d/v", reg))
  expect_identical(hist, ref)

  # E retomou de VERDADE: 150 pontos no segundo run, não 250. Sem isto o
  # `expect_identical` passaria com um run do zero, que é justamente o que esta
  # tarefa existe pra evitar.
  expect_equal(length(e$puros) - antes, 150L)

  # 4. Concluída a unidade, o diretório sai: o artefato final o substitui.
  expect_false(dir.exists(dirname(ckpt_path(s, u$key))))
})

test_that("sem `estado` no checkpoint a retomada mentiria: a média corrente não reinicia", {
  # A metade do `expect_identical` acima que um checkpoint só de `hist` deixaria
  # passar sem ruído: os 250 pontos estariam lá, e a média a partir do 101 seria
  # a média dos ÚLTIMOS 150 — curva plausível, gravada no store, servida do
  # cache. Aqui a asserção é sobre o VALOR, ponto a ponto.
  e <- diario(); reg <- driver_registry(e)
  doc <- doc_ckpt(reg)
  s <- tmp_store()
  u <- tr_plan(doc, registry = reg, store = s)$units$co

  e$morre_em <- 180
  expect_error(.tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 100)),
               class = "tr_error_stream_step")
  e$morre_em <- NULL
  .tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 100))

  hist <- tr_store_get(s, u$outputs$out, tr_get_type("d/v", reg))
  expect_equal(hist$v, cumsum(1:250) / seq_len(250))
})

test_that("`checkpoint_every = 0` desliga o checkpoint, e nada é gravado", {
  # A válvula de quem tem histórico gigante: cada checkpoint serializa o
  # acumulador inteiro, e num fluxo cujo ponto é pesado isso é a maior escrita
  # do run. Desligar tem que ser possível — e tem que não deixar rastro.
  e <- diario(); reg <- driver_registry(e)
  s <- tmp_store()
  u <- tr_plan(doc_ckpt(reg), registry = reg, store = s)$units$co
  e$morre_em <- 180
  expect_error(.tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 0)),
               class = "tr_error_stream_step")
  expect_false(file.exists(ckpt_path(s, u$key)))

  # E sem checkpoint a retomada é o run do zero: 250 pontos de novo.
  e$morre_em <- NULL
  antes <- length(e$puros)
  .tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 0))
  expect_equal(length(e$puros) - antes, 250L)
})

test_that("checkpoint ilegível conta como ausente, nunca como exceção", {
  # O mesmo argumento de `tr_store_handle()`: um único arquivo truncado não pode
  # deixar uma região PERMANENTEMENTE não-rodável. `.tr_atomic()` é que impede o
  # truncado de existir; esta é a rede embaixo dela, e o custo de cair nela é um
  # run do zero — não um erro.
  e <- diario(); reg <- driver_registry(e)
  s <- tmp_store()
  u <- tr_plan(doc_ckpt(reg, n = 10L), registry = reg, store = s)$units$co
  dir.create(dirname(ckpt_path(s, u$key)), recursive = TRUE, showWarnings = FALSE)
  writeLines("isto não é um RDS", ckpt_path(s, u$key))

  expect_silent(.tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 5)))
  hist <- tr_store_get(s, u$outputs$out, tr_get_type("d/v", reg))
  expect_equal(hist$v, cumsum(1:10) / seq_len(10))
})

test_that("checkpoint com outra contagem de pontos é descartado", {
  # A fonte é isenta da liftabilidade, então ela PODE ser impura: um
  # `data/to_stream` que lê arquivo devolve 300 pontos hoje e 250 amanhã sob a
  # MESMA chave. Retomar com o acumulador do tamanho errado misturaria dois
  # fluxos — e sairia um histórico plausível, do tamanho certo, com valores de
  # duas leituras diferentes.
  e <- diario(); reg <- driver_registry(e)
  s <- tmp_store()
  u <- tr_plan(doc_ckpt(reg, n = 10L), registry = reg, store = s)$units$co
  dir.create(dirname(ckpt_path(s, u$key)), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(i = 5L, n = 999L, estado = list(), hist = list()),
          ckpt_path(s, u$key))

  .tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 5))
  hist <- tr_store_get(s, u$outputs$out, tr_get_type("d/v", reg))
  expect_equal(hist$v, cumsum(1:10) / seq_len(10))
  expect_equal(length(e$puros), 10L)   # rodou do zero, e não do passo 6
})

test_that("região de zero pontos não deixa checkpoint nem diretório", {
  e <- diario(); reg <- driver_registry(e)
  s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 0L) |>
    tr_add("co", "d/colapsa", from = "fo"))
  u <- tr_plan(doc, registry = reg, store = s)$units$co
  .tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 1))
  expect_false(dir.exists(dirname(ckpt_path(s, u$key))))
})

test_that("falha no COLAPSO guarda o checkpoint do último múltiplo, e o laço não roda de novo inteiro", {
  # O laço terminou e o colapso explodiu: perder o checkpoint aqui custaria os
  # 250 pontos por causa de um erro que aconteceu DEPOIS deles. O que sobra é o
  # último múltiplo da cadência, porque o passo final não gasta uma escrita do
  # acumulador inteiro (ver o comentário do driver).
  e <- diario(); reg <- driver_registry(e)
  s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 250L) |>
    tr_add("mo", "d/puro_morre", from = "fo") |>
    tr_add("co", "d/colapsa_explode", from = "mo"))
  u <- tr_plan(doc, registry = reg, store = s)$units$co

  expect_error(.tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 100)),
               class = "tr_error_stream_collapse")
  expect_equal(readRDS(ckpt_path(s, u$key))$i, 200L)
})

test_that("pelo run completo: morre, o handle de erro é bustado, e a retomada não repete o laço", {
  # O caminho de verdade — plano, scheduler, executor, store. O executor
  # sequencial não tem daemon a matar, então a morte aqui é o erro injetado no
  # membro; o que fica sem medida é o kill de processo do `tr_executor_pool()`,
  # que a coleção `d/*` não alcança (sem `package`, o pool a recusa).
  #
  # `tr_bust()` no meio não é decoração: o `collect()` grava o erro sob as
  # chaves de saída da região, e nenhum `tr_plan()` recalcula um handle que
  # existe — sem bustar, o segundo run não despacharia nada. É também a razão de
  # `tr_bust()` apagar `stream/`: bustar é dizer "o código mudou sem a chave
  # mudar", e retomar de um checkpoint feito pelo código de antes serviria um
  # histórico metade velho, metade novo.
  e <- diario(); reg <- driver_registry(e)
  doc <- doc_ckpt(reg)
  s0 <- tmp_store()
  u0 <- tr_plan(doc, registry = reg, store = s0)$units$co
  tr_run(doc, registry = reg, store = s0)
  ref <- tr_store_get(s0, u0$outputs$out, tr_get_type("d/v", reg))

  s <- tmp_store()
  u <- tr_plan(doc, registry = reg, store = s)$units$co
  e$morre_em <- 180
  r1 <- tr_run(doc, registry = reg, store = s)
  expect_true("co" %in% r1$skipped)
  expect_true(file.exists(ckpt_path(s, u$key)))
  ck <- readRDS(ckpt_path(s, u$key))
  expect_equal(ck$i, 100L)

  # Só os handles de erro saem; o checkpoint fica onde está.
  for (k in unlist(u$outputs)) unlink(file.path(s$root, "handles", paste0(k, ".json")))
  e$morre_em <- NULL
  antes <- length(e$puros)
  r2 <- tr_run(doc, registry = reg, store = s)
  expect_true("co" %in% r2$done)
  expect_identical(tr_store_get(s, u$outputs$out, tr_get_type("d/v", reg)), ref)
  expect_equal(length(e$puros) - antes, 150L)
})

test_that("tr_bust apaga os checkpoints: retomar com o código novo é pior que recomputar", {
  e <- diario(); reg <- driver_registry(e)
  s <- tmp_store()
  u <- tr_plan(doc_ckpt(reg), registry = reg, store = s)$units$co
  e$morre_em <- 180
  expect_error(.tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 100)),
               class = "tr_error_stream_step")
  expect_true(file.exists(ckpt_path(s, u$key)))

  tr_bust(s)
  expect_false(file.exists(ckpt_path(s, u$key)))
})

test_that("tr_store_gc varre `stream/` por IDADE, e não pelo `keep`", {
  # `keep` são as chaves de SAÍDA (`tr_plan_keys()`), e o checkpoint mora sob a
  # chave da UNIDADE — que nunca aparece lá. A idade é o critério melhor de
  # qualquer forma: um checkpoint vivo é reescrito a cada cadência, então só o
  # morto fica velho. Sem esta varredura cada região abandonada deixa no disco o
  # acumulador inteiro dela, num store que já cresce sem limite por construção.
  s <- tmp_store()
  d <- file.path(s$root, "stream", "chave_morta")
  dir.create(d, recursive = TRUE)
  saveRDS(list(i = 1L), file.path(d, "ckpt.rds"))
  novo <- file.path(s$root, "stream", "chave_viva")
  dir.create(novo, recursive = TRUE)
  saveRDS(list(i = 1L), file.path(novo, "ckpt.rds"))
  # Só o velho envelhece.
  Sys.setFileTime(file.path(d, "ckpt.rds"), Sys.time() - 10 * 86400)
  Sys.setFileTime(d, Sys.time() - 10 * 86400)

  tr_store_gc(s, max_age_days = 7)
  expect_false(dir.exists(d))
  expect_true(dir.exists(novo))
})

test_that("`checkpoint_every` torto desliga o checkpoint, e não derruba a região", {
  # Mesma régua do `tryCatch` no tipo do parcial: uma região que roda hoje não
  # pode parar de rodar por causa de um botão de operação. Desligado é o
  # comportamento de antes desta tarefa; abortar o laço de dez mil pontos por
  # causa de um `checkpoint_every` mal digitado não é.
  e <- diario(); reg <- driver_registry(e)
  for (torto in list(NA, "cem", -5L, c(10L, 20L))) {
    s <- tmp_store()
    u <- tr_plan(doc_ckpt(reg, n = 10L), registry = reg, store = s)$units$co
    .tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = torto))
    hist <- tr_store_get(s, u$outputs$out, tr_get_type("d/v", reg))
    expect_equal(hist$v, cumsum(1:10) / seq_len(10))
    expect_false(dir.exists(dirname(ckpt_path(s, u$key))))
  }
})

# --- `.seed` dentro da região ------------------------------------------------
#
# O furo que esta seção fecha: `.tr_region_args()` montava só entradas e params,
# e `.seed` NUNCA chegava a membro nenhum. Um membro que não forçava o argumento
# rodava calado, com o RNG global do daemon; um que o forçava morria com
# "argumento '.seed' ausente, sem padrão".
#
# Calado é o pior dos dois, e é por isso que isto é furo de CORREÇÃO e não
# funcionalidade que falta: `.tr_region_key()` JÁ inclui a seed de um membro
# estocástico (`hash.R`), então a chave promete que o histórico depende dela
# enquanto o histórico dependia, de fato, do RNG do processo que pegou a
# unidade. A mesma chave podia servir dois históricos diferentes — e uma região
# do cache discordar de uma recomputada.
#
# A regra implementada: o membro elevado (e o `step` do membro com memória)
# recebe uma seed DERIVADA de (id do membro, seed dele, índice do passo); a
# FONTE e o COLAPSO, que rodam uma vez, recebem a própria seed do documento.
#
# `seed = ` explícito em todo `tr_add()` daqui pra baixo: `.tr_new_seed()` é
# baseado em entropia, então sem isso dois documentos "iguais" teriam seeds
# diferentes e nenhum teste de reprodutibilidade mediria o que promete.

test_that("membro elevado estocástico sorteia DIFERENTE em cada passo", {
  # A falha plausível que o desenho da seed derivada existe pra evitar: com a
  # seed CRUA do membro em todos os passos, `set.seed()` reiniciaria o RNG a
  # cada ponto e os n sorteios sairiam idênticos — um "ruído" que é uma
  # constante, e um histórico que nada denuncia.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 5L) |>
    tr_add("so", "d/sorteia", from = "fo", seed = 11L) |>
    tr_add("co", "d/colapsa", from = "so"))

  r <- roda_regiao(doc, reg, s)
  hist <- tr_store_get(s, r$unit$outputs$out, tr_get_type("d/v", reg))
  expect_equal(nrow(hist), 5L)
  expect_equal(length(unique(hist$v)), 5L)
  # E as cinco seeds são cinco, não a mesma cinco vezes — nem a seed do nó.
  seeds <- unlist(e$seeds)
  expect_equal(length(unique(seeds)), 5L)
  expect_false(any(seeds == 11L))
})

test_that("o MESMO documento produz o MESMO histórico, duas vezes", {
  # A propriedade inteira. Dois stores, dois runs, nenhum `set.seed()` externo:
  # se a seed do passo viesse do RNG do processo, os dois históricos divergiriam
  # sob a MESMA chave de região.
  reg1 <- driver_registry(diario()); reg2 <- driver_registry(diario())
  doc <- tr_flow_doc(tr_flow(reg1) |>
    tr_add("fo", "d/fonte", n = 20L) |>
    tr_add("so", "d/sorteia", from = "fo", seed = 4242L) |>
    tr_add("co", "d/colapsa", from = "so"))

  s1 <- tmp_store(); u1 <- tr_plan(doc, registry = reg1, store = s1)$units$co
  set.seed(1); .tr_run_unit(u1, reg1, s1)
  h1 <- tr_store_get(s1, u1$outputs$out, tr_get_type("d/v", reg1))

  s2 <- tmp_store(); u2 <- tr_plan(doc, registry = reg2, store = s2)$units$co
  # RNG global em OUTRO estado de propósito: é ele que não pode aparecer no
  # histórico.
  set.seed(999); .tr_run_unit(u2, reg2, s2)
  h2 <- tr_store_get(s2, u2$outputs$out, tr_get_type("d/v", reg2))

  expect_identical(u1$key, u2$key)
  expect_identical(h1, h2)
})

test_that("dois membros no mesmo passo sorteiam diferente, e o mesmo membro em dois passos também", {
  # A propriedade de NÃO-COLISÃO, e é por ela que `seed + i` não serve: o membro
  # A com seed 1 no passo 2 colidiria com o membro B com seed 2 no passo 1, e os
  # dois sorteariam igual — dois detectores "independentes" vendo o mesmo ruído.
  # Aqui os dois membros levam a MESMA seed de documento, então o que os separa
  # só pode ser a identidade deles.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 4L) |>
    tr_add("s1", "d/sorteia", from = "fo", seed = 7L) |>
    tr_add("s2", "d/sorteia", from = "s1", seed = 7L) |>
    tr_add("co", "d/colapsa", from = "s2"))

  roda_regiao(doc, reg, s)
  seeds <- unlist(e$seeds)
  # Oito chamadas: dois membros por passo, quatro passos, na ordem do laço.
  expect_equal(length(seeds), 8L)
  # Mesmo passo, membros diferentes.
  expect_false(seeds[[1]] == seeds[[2]])
  # Mesmo membro, passos diferentes.
  expect_false(seeds[[1]] == seeds[[3]])
  # E nenhum par dos oito coincide.
  expect_equal(length(unique(seeds)), 8L)
})

test_that("o `step` de um membro COM MEMÓRIA recebe `.seed`, um por passo", {
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 6L) |>
    tr_add("ac", "d/soma_sorteios", from = "fo", seed = 5L) |>
    tr_add("co", "d/colapsa", from = "ac"))

  r <- roda_regiao(doc, reg, s)
  seeds <- unlist(e$seeds_step)
  expect_equal(length(seeds), 6L)
  expect_true(is.integer(seeds))
  expect_false(anyNA(seeds))
  expect_equal(length(unique(seeds)), 6L)
  # A soma corrente cresce: seis sorteios distintos, não seis vezes o mesmo.
  hist <- tr_store_get(s, r$unit$outputs$out, tr_get_type("d/v", reg))
  expect_equal(length(unique(diff(hist$v))), 5L)
})

test_that("a FONTE recebe a PRÓPRIA seed do documento, não uma derivada", {
  # O `fn` da fonte roda UMA vez, antes do laço — dentro da região ou solto no
  # nível 1 da API. Passar outra coisa que não `node$seed` faria o MESMO nó se
  # comportar diferente nos dois lugares, e "a seed desta invocação" não tem o
  # que variar quando há uma invocação só.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte_seed", n = 3L, seed = 31337L) |>
    tr_add("pu", "d/puro", from = "fo") |>
    tr_add("co", "d/colapsa", from = "pu"))

  roda_regiao(doc, reg, s)
  expect_identical(e$seed_fonte, 31337L)
})

test_that("retomada com membro estocástico sai IDÊNTICA ao run sem interrupção", {
  # O teste importante. O checkpoint da Tarefa 5.2 NÃO guarda o estado do RNG,
  # de propósito: restaurar `.Random.seed` num daemon vazaria para a próxima
  # unidade que rodasse ali. Com a seed derivada por passo isso não custa nada —
  # a seed de cada passo é função pura de (membro, passo), e não do histórico do
  # RNG —, e é essa pureza que este `expect_identical` mede. Se ele falhar, a
  # derivação não é pura e o desenho tem que voltar à mesa.
  e <- diario(); reg <- driver_registry(e)
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 250L) |>
    tr_add("mo", "d/puro_morre", from = "fo") |>
    tr_add("so", "d/sorteia", from = "mo", seed = 2024L) |>
    tr_add("ac", "d/soma_sorteios", from = "so", seed = 808L) |>
    tr_add("co", "d/colapsa", from = "ac"))

  s0 <- tmp_store()
  u0 <- tr_plan(doc, registry = reg, store = s0)$units$co
  .tr_run_unit(u0, reg, s0, ctx_extra = list(checkpoint_every = 100))
  ref <- tr_store_get(s0, u0$outputs$out, tr_get_type("d/v", reg))
  expect_equal(nrow(ref), 250L)

  s <- tmp_store()
  u <- tr_plan(doc, registry = reg, store = s)$units$co
  expect_identical(u$key, u0$key)
  e$morre_em <- 180
  expect_error(.tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 100)),
               class = "tr_error_stream_step")
  expect_equal(readRDS(ckpt_path(s, u$key))$i, 100L)

  e$morre_em <- NULL
  antes <- length(e$seeds)
  .tr_run_unit(u, reg, s, ctx_extra = list(checkpoint_every = 100))
  hist <- tr_store_get(s, u$outputs$out, tr_get_type("d/v", reg))
  expect_identical(hist, ref)
  # E retomou de verdade: 150 sorteios no segundo run, não 250.
  expect_equal(length(e$seeds) - antes, 150L)
})

test_that("membro que declara `.seed` e não o força não roda mais com o RNG global", {
  # O sintoma medido era o SILÊNCIO: a avaliação preguiçosa do R nunca reclama
  # de um argumento que o corpo não toca, então um teste que só "roda" não
  # distingue nada aqui. `missing()` pergunta se o CHAMADOR passou, sem forçar —
  # antes desta tarefa era TRUE em todos os passos, e o nó sorteava do RNG do
  # daemon sob uma chave que promete depender da seed.
  e <- diario(); reg <- driver_registry(e); s <- tmp_store()
  doc <- tr_flow_doc(tr_flow(reg) |>
    tr_add("fo", "d/fonte", n = 4L) |>
    tr_add("so", "d/sorteia_frouxo", from = "fo", seed = 3L) |>
    tr_add("co", "d/colapsa", from = "so"))

  roda_regiao(doc, reg, s)
  expect_equal(length(e$faltou), 4L)
  expect_false(any(e$faltou))
})
