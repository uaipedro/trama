# A região de fluxo como UNIDADE do plano — o cruzamento do fluxo com o modelo
# de cache. Todo teste de chave aqui existe contra o mesmo modo de falha: a
# região servir, do cache, um histórico que não corresponde mais ao que o
# documento manda calcular. Errado em silêncio, para sempre.

# Coleção paramétrica: `init`, `step`, o adaptador e o `preview` do tipo entram
# por argumento porque os testes de chave precisam de DOIS registros que diferem
# em exatamente um deles. Com a coleção fixa de `helper-collection.R` não há
# como editar o corpo de um `step` sem editar o helper que os outros arquivos
# usam.
mem_collection <- function(init = function(peso) list(soma = 0, peso = peso),
                           step = function(state, x) list(state = state, out = x),
                           adapter = function(x) x,
                           preview = function(x, ctx) tr_preview("m/tab", data = list(v = 1))) {
  ponto <- function(...) tr_port("m/tab", stream = TRUE, ...)

  tr_collection(
    id = "m", version = "1.0.0", label = "Fluxo com memória",
    types = list(tr_type("m/tab", label = "Tabela", preview = preview),
                 tr_type("m/pt", label = "Ponto")),
    nodes = list(
      tr_node("m/tabela", fn = function(v) v, outputs = list(out = "m/tab"),
              params = list(v = tr_param_num(1)),
              description = "Tabela comum, com um param pra mexer."),
      tr_node("m/fonte", fn = function(dados) dados,
              inputs = list(dados = tr_port("m/tab", required = FALSE)),
              outputs = list(out = ponto()),
              description = "Parte a tabela em pontos."),
      tr_node("m/puro", fn = function(x, fator) x,
              inputs = list(x = "m/tab"), outputs = list(out = "m/tab"),
              params = list(fator = tr_param_num(0)),
              description = "Nó comum, elevado ponto a ponto dentro da região."),
      tr_node("m/junta", fn = function(a, b) a,
              inputs = list(a = "m/tab", b = "m/tab"), outputs = list(out = "m/tab"),
              description = "Combina dois pontos — a ordem das portas importa."),
      tr_node("m/acc", fn = function(x, k) x,
              inputs = list(x = ponto(), k = tr_port("m/tab", required = FALSE)),
              outputs = list(out = ponto()),
              params = list(peso = tr_param_num(1)),
              init = init, step = step,
              description = "Acumula ponto a ponto."),
      tr_node("m/acc_impuro", fn = function(x, path) x,
              inputs = list(x = ponto()), outputs = list(out = ponto()),
              params = list(path = tr_param_text("")),
              pure = FALSE,
              fingerprint = function(params) {
                i <- file.info(params$path)
                paste0(params$path, ":", i$size, ":", i$mtime)
              },
              init = function() list(n = 0),
              step = function(state, x) list(state = state, out = x),
              description = "Acumula lendo um arquivo fora do grafo."),
      tr_node("m/acc_volatil", fn = function(x) x,
              inputs = list(x = ponto()), outputs = list(out = ponto()),
              volatile = TRUE,
              init = function() list(n = 0),
              step = function(state, x) list(state = state, out = x),
              description = "Acumula sem nunca reaproveitar execução anterior."),
      tr_node("m/colapsa", fn = function(x) x, inputs = list(x = ponto()),
              outputs = list(out = "m/tab"),
              description = "Junta os pontos num histórico."),
      tr_node("m/colapsa_pt", fn = function(x) x,
              inputs = list(x = tr_port("m/pt", stream = TRUE)),
              outputs = list(out = "m/tab"),
              description = "Colapsa pontos de outro tipo — a aresta interna pede adaptador."),
      tr_node("m/mostra", fn = function(x) invisible(x), inputs = list(x = "m/tab"),
              description = "Mostra a tabela recebida."),
      tr_node("m/exibe", fn = function(x) invisible(x), inputs = list(x = ponto()),
              description = "Colapsa o fluxo sem devolver valor: só exibe.")
    ),
    adapters = list(tr_adapter("m/tab", "m/pt", adapter))
  )
}

mem_registry <- function(...) {
  reg <- tr_registry(); tr_use(mem_collection(...), registry = reg); reg
}

# fonte -> acc -> colapsa, com a tabela de fora entrando na fonte e um
# consumidor depois do colapso: o grafo mínimo que tem tudo o que a Fase 3
# precisa decidir.
mem_flow <- function(reg) {
  tr_flow(reg) |>
    tr_add("tab", "m/tabela") |>
    tr_add("fonte", "m/fonte", from = "tab") |>
    tr_add("acumula", "m/acc", from = "fonte") |>
    tr_add("colapsa", "m/colapsa", from = "acumula") |>
    tr_add("depois", "m/mostra", from = "colapsa")
}

mem_plan <- function(reg, flow = mem_flow(reg), ...) {
  tr_plan(tr_flow_doc(flow), registry = reg, ...)
}

chave <- function(reg, flow) mem_plan(reg, flow)$units[["colapsa"]]$key

# --- A região como uma unidade ----------------------------------------------

test_that("a região vira UMA unidade, com as saídas do colapso", {
  reg <- mem_registry()
  plan <- mem_plan(reg)

  # 3 nós de região (fonte, acumula, colapsa) + tabela de fora + consumidor =
  # 3 unidades, não 5.
  expect_length(plan$units, 3L)
  expect_setequal(names(plan$units), c("tab", "colapsa", "depois"))

  u <- plan$units[["colapsa"]]
  expect_equal(u$kind, "stream_region")
  expect_equal(u$node, "colapsa")
  expect_equal(u$node_type, "trama/stream_region")
  expect_named(u$outputs, "out")
  expect_equal(u$output_types, c(out = "m/tab"))
  expect_equal(u$region$order, c("fonte", "acumula", "colapsa"))
  expect_equal(u$region$source, "fonte")
  expect_equal(u$region$collapse, "colapsa")

  # O consumidor lê a saída do colapso pela chave da unidade-região, sem saber
  # que existe região: é o que a fase promete ao resto do plano.
  expect_equal(plan$units[["depois"]]$inputs$x$key, u$outputs$out)
  expect_equal(plan$units[["depois"]]$inputs$x$node, "colapsa")

  # Ordem topológica: o montante externo antes da região, a região antes do
  # consumidor.
  expect_lt(which(names(plan$units) == "tab"), which(names(plan$units) == "colapsa"))
  expect_lt(which(names(plan$units) == "colapsa"), which(names(plan$units) == "depois"))
})

test_that("nó interior não tem unidade nem chave", {
  plan <- mem_plan(mem_registry())
  expect_null(plan$units[["acumula"]])
  expect_null(plan$units[["fonte"]])
  expect_null(plan$keys[["acumula"]])
  # E o GC não guarda chave nenhuma para nó interior: quem só existe dentro da
  # região não grava artefato.
  expect_setequal(tr_plan_keys(plan),
                  c(plan$units$tab$outputs$out, plan$units$colapsa$outputs$out,
                    plan$units$depois$outputs[[1]]))
})

test_that("a unidade-região não carrega fn nem environment", {
  u <- mem_plan(mem_registry())$units[["colapsa"]]
  expect_null(u$spec)
  for (m in u$region$nodes) expect_null(m$fn)
  expect_type(jsonlite::toJSON(u$outputs, auto_unbox = TRUE), "character")
})

test_that("a unidade-região traz o que o worker precisa de cada nó", {
  reg <- mem_registry()
  f <- tr_flow(reg) |>
    tr_add("tab", "m/tabela") |>
    tr_add("modelo", "m/tabela") |>
    tr_add("fonte", "m/fonte", from = "tab") |>
    tr_add("acumula", "m/acc", from = "fonte", peso = 3) |>
    tr_link("modelo:out", "acumula:k") |>
    tr_add("colapsa", "m/colapsa", from = "acumula")
  u <- mem_plan(reg, f)$units[["colapsa"]]

  m <- u$region$nodes[["acumula"]]
  expect_equal(m$node_type, "m/acc")
  expect_true(m$online)
  expect_equal(m$params$peso, 3)
  expect_equal(m$role, "lifted")
  expect_equal(u$region$nodes[["fonte"]]$role, "source")
  expect_equal(u$region$nodes[["colapsa"]]$role, "collapse")

  # A porta `x` vem de DENTRO da região; a porta `k` vem de fora, e aponta
  # para a entrada externa da unidade.
  expect_equal(m$inputs$x[[1]]$from, "internal")
  expect_equal(m$inputs$x[[1]]$node, "fonte")
  expect_equal(m$inputs$x[[1]]$port, "out")
  expect_equal(m$inputs$k[[1]]$from, "external")
  expect_equal(m$inputs$k[[1]]$input, "acumula:k")

  # As entradas externas são referências no formato de sempre — chave, tipo e
  # adaptador — só que nomeadas por nó.porta, porque uma região tem portas de
  # nós diferentes.
  expect_setequal(names(u$inputs), c("fonte:dados", "acumula:k"))
  expect_equal(u$inputs[["acumula:k"]]$node, "modelo")
  expect_equal(u$inputs[["acumula:k"]]$type, "m/tab")
  expect_null(u$inputs[["acumula:k"]]$adapter)
})

test_that("a região pede '.ctx' e não pede '.seed'", {
  u <- mem_plan(mem_registry())$units[["colapsa"]]
  # O driver da região publica parcial por nó (Fase 5): `.ctx` é contrato da
  # UNIDADE, não de nenhum `fn` membro — nó elevado tem `.ctx` proibido.
  expect_true(u$wants_ctx)
  # Não existe "a seed da região": cada membro leva a sua no payload.
  expect_false(u$wants_seed)
})

test_that("plan$region_of aponta cada membro para a unidade que o executa", {
  plan <- mem_plan(mem_registry())
  expect_equal(plan$region_of[["acumula"]], "colapsa")
  expect_equal(plan$region_of[["fonte"]], "colapsa")
  expect_equal(plan$region_of[["colapsa"]], "colapsa")
  expect_false("tab" %in% names(plan$region_of))
})

test_that("documento sem fluxo nenhum sai exatamente como antes", {
  reg <- store_registry()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "c"),
    list(op = "add_node", type = "t/inc", id = "i"),
    list(op = "connect", from_node = "c", from_port = "out", to_node = "i", to_port = "x")))
  plan <- tr_plan(doc, registry = reg)
  expect_setequal(names(plan$units), c("c", "i"))
  expect_equal(plan$units$i$kind, "node")
  expect_length(plan$region_of, 0L)
})

# --- A chave da região -------------------------------------------------------

test_that("mudar param de QUALQUER nó da região muda a chave da unidade", {
  reg <- mem_registry()
  k1 <- chave(reg, mem_flow(reg))
  k2 <- chave(reg, tr_set(mem_flow(reg), "acumula", peso = 2))
  expect_false(identical(k1, k2))
})

test_that("mudar nó FORA da região não muda a chave da região", {
  reg <- mem_registry()
  base <- tr_flow(reg) |>
    tr_add("tab", "m/tabela") |>
    tr_add("fonte", "m/fonte", from = "tab") |>
    tr_add("colapsa", "m/colapsa", from = "fonte") |>
    tr_add("depois", "m/puro", from = "colapsa") |>
    tr_add("solto", "m/tabela")

  k1 <- chave(reg, base)
  # Nó a jusante do colapso e nó desligado: nenhum dos dois entra no que a
  # região calcula.
  expect_equal(chave(reg, tr_set(base, "depois", fator = 9)), k1)
  expect_equal(chave(reg, tr_set(base, "solto", v = 9)), k1)
})

test_that("a chave da região depende da chave de montante externa", {
  reg <- mem_registry()
  k1 <- chave(reg, mem_flow(reg))
  k2 <- chave(reg, tr_set(mem_flow(reg), "tab", v = 7))
  expect_false(identical(k1, k2))
})

test_that("região a jusante de região: a chave da segunda depende da primeira", {
  reg <- mem_registry()
  f <- function(v) {
    tr_flow(reg) |>
      tr_add("tab", "m/tabela", v = v) |>
      tr_add("f1", "m/fonte", from = "tab") |>
      tr_add("c1", "m/colapsa", from = "f1") |>
      tr_add("f2", "m/fonte", from = "c1") |>
      tr_add("c2", "m/colapsa", from = "f2")
  }
  p1 <- mem_plan(reg, f(1)); p2 <- mem_plan(reg, f(2))
  expect_length(p1$units, 3L)                       # tab + duas regiões
  expect_setequal(names(p1$units), c("tab", "c1", "c2"))
  expect_equal(p1$units$c2$inputs[["f2:dados"]]$key, p1$units$c1$outputs$out)
  expect_false(identical(p1$units$c2$key, p2$units$c2$key))
})

test_that("mudar o corpo de `step` muda a chave da região", {
  # A armadilha da Fase 1: `.tr_unit_key()` monta a chave com `fn_print`, que
  # sai só de `spec$fn`. Um nó com memória roda por `init`/`step` — se eles
  # ficassem fora do fingerprint, editar a lógica do acumulador serviria o
  # histórico velho do cache, em silêncio, para sempre.
  r1 <- mem_registry(step = function(state, x) list(state = state, out = x))
  r2 <- mem_registry(step = function(state, x) list(state = state, out = rev(x)))
  expect_false(identical(chave(r1, mem_flow(r1)), chave(r2, mem_flow(r2))))
})

test_that("mudar o corpo de `init` muda a chave da região", {
  r1 <- mem_registry(init = function(peso) list(soma = 0, peso = peso))
  r2 <- mem_registry(init = function(peso) list(soma = 100, peso = peso))
  expect_false(identical(chave(r1, mem_flow(r1)), chave(r2, mem_flow(r2))))
})

test_that("membro volátil torna a chave da região irrepetível", {
  # Buraco herdado da Fase 2: a liftabilidade isenta nó `online` por completo,
  # então um nó com memória volátil NÃO é recusado. Sem tratá-lo aqui, a região
  # cachearia um histórico que tem que ser recomputado sempre.
  reg <- mem_registry()
  f <- tr_flow(reg) |>
    tr_add("fonte", "m/fonte") |>
    tr_add("acumula", "m/acc_volatil", from = "fonte") |>
    tr_add("colapsa", "m/colapsa", from = "acumula")
  expect_false(identical(chave(reg, f), chave(reg, f)))

  # E o contraste: sem membro volátil, a chave é estável entre dois planos.
  expect_equal(chave(reg, mem_flow(reg)), chave(reg, mem_flow(reg)))
})

test_that("membro impuro leva o fingerprint externo na chave da região", {
  # O outro lado do mesmo buraco: nó com memória `pure = FALSE` passa pela
  # liftabilidade. Sem o `fingerprint()` dele na chave, a região serve dado
  # velho quando o arquivo que ele lê muda.
  reg <- mem_registry()
  arq <- tempfile(); writeLines("1", arq)
  f <- tr_flow(reg) |>
    tr_add("fonte", "m/fonte") |>
    tr_add("acumula", "m/acc_impuro", from = "fonte", path = arq) |>
    tr_add("colapsa", "m/colapsa", from = "acumula")

  k1 <- chave(reg, f)
  Sys.sleep(1.1); writeLines("22", arq)            # mesmo caminho, conteúdo novo
  expect_false(identical(chave(reg, f), k1))
})

test_that("religar aresta INTERNA muda a chave da região", {
  # A topologia interna não é params nem código: `junta(a = , b = )` com as
  # portas trocadas é outra computação. Sem as arestas internas na chave, o
  # documento religado serviria o resultado da ligação antiga.
  reg <- mem_registry()
  f <- function(troca) {
    fl <- tr_flow(reg) |>
      tr_add("fonte", "m/fonte") |>
      tr_add("p", "m/puro", from = "fonte") |>
      tr_add("junta", "m/junta")
    fl <- if (troca) {
      fl |> tr_link("fonte:out", "junta:b") |> tr_link("p:out", "junta:a")
    } else {
      fl |> tr_link("fonte:out", "junta:a") |> tr_link("p:out", "junta:b")
    }
    fl |> tr_add("colapsa", "m/colapsa", from = "junta")
  }
  expect_false(identical(chave(reg, f(FALSE)), chave(reg, f(TRUE))))
})

test_that("adaptador de aresta INTERNA entra na chave da região", {
  # O adaptador roda dentro da região, ponto a ponto, e o resultado dele vira o
  # histórico cacheado: mudar o corpo dele sem mudar a chave serve histórico
  # convertido pela regra antiga.
  f <- function(reg) {
    tr_flow(reg) |>
      tr_add("fonte", "m/fonte") |>
      tr_add("colapsa", "m/colapsa_pt", from = "fonte")
  }
  r1 <- mem_registry(adapter = function(x) x)
  r2 <- mem_registry(adapter = function(x) rev(x))
  p1 <- mem_plan(r1, f(r1))
  expect_equal(p1$units$colapsa$region$nodes$colapsa$inputs$x[[1]]$adapter,
               list(from = "m/tab", to = "m/pt"))
  expect_false(identical(chave(r1, f(r1)), chave(r2, f(r2))))
})

test_that("mudar o preview do TIPO da saída do colapso invalida a chave", {
  r1 <- mem_registry(preview = function(x, ctx) tr_preview("m/tab", data = list(cor = "vermelho")))
  r2 <- mem_registry(preview = function(x, ctx) tr_preview("m/tab", data = list(cor = "azul")))
  expect_false(identical(chave(r1, mem_flow(r1)), chave(r2, mem_flow(r2))))
})

test_that("mexer no documento sem mexer na região não muda a chave", {
  reg <- mem_registry()
  base <- mem_flow(reg)
  k <- chave(reg, base)
  # Renomear e mover são apresentação, aqui como em qualquer nó.
  doc <- tr_doc_apply(tr_flow_doc(base), list(op = "rename", node = "acumula", label = "Outro"), reg)
  doc <- tr_doc_apply(doc, list(op = "move", node = "acumula", x = 99, y = 99), reg)
  expect_equal(tr_plan(doc, registry = reg)$units[["colapsa"]]$key, k)
  # E salvar/reabrir também não.
  expect_equal(tr_plan(tr_doc_parse(tr_doc_json(tr_flow_doc(base))), registry = reg)$units[["colapsa"]]$key, k)
})

test_that("a chave da região não depende da locale", {
  reg <- mem_registry()
  f <- mem_flow(reg)
  k1 <- withr::with_collate("C", chave(reg, f))
  k2 <- withr::with_collate("en_US.UTF-8", chave(reg, f))
  expect_equal(k1, k2)
})

# --- Cache, bloqueio e porta solta ------------------------------------------

test_that("região em cache é decidida no plano, como qualquer unidade", {
  reg <- mem_registry(); s <- tmp_store()
  f <- mem_flow(reg)
  p1 <- mem_plan(reg, f, store = s)
  expect_false(p1$units$colapsa$cached)

  tr_store_put(s, p1$units$colapsa$outputs$out, 1, tr_get_type("m/tab", reg),
               node_type = "trama/stream_region")
  p2 <- mem_plan(reg, f, store = s)
  expect_true(p2$units$colapsa$cached)
  expect_false(is.null(p2$units$colapsa$handles))
  expect_false("colapsa" %in% vapply(tr_plan_pending(p2), function(u) u$node, ""))
})

test_that("montante externo com erro bloqueia a região em vez de rodá-la", {
  reg <- mem_registry(); s <- tmp_store()
  f <- mem_flow(reg)
  p <- mem_plan(reg, f, store = s)
  tr_store_put_error(s, p$units$tab$outputs$out, "explodiu", node_type = "m/tabela")

  p2 <- mem_plan(reg, f, store = s)
  expect_true(p2$units$tab$failed)
  expect_equal(p2$units$colapsa$blocked_by, "tab")
  # E o bloqueio se propaga para quem consome o colapso.
  expect_equal(p2$units$depois$blocked_by, "colapsa")
  expect_setequal(vapply(tr_plan_blocked(p2), function(u) u$node, ""),
                  c("tab", "colapsa", "depois"))
})

test_that("porta obrigatória solta DENTRO da região invalida a unidade", {
  # Sem isto a região entraria na fila e o worker falharia com "argumento
  # ausente, sem padrão" no meio do driver — o erro longe da causa.
  reg <- mem_registry()
  f <- tr_flow(reg) |>
    tr_add("fonte", "m/fonte") |>
    tr_add("p", "m/puro") |>
    tr_add("colapsa", "m/colapsa", from = "fonte") |>
    tr_link("fonte:out", "p:x")
  # `p` é membro elevado (come fluxo) e a saída dele não sai da região.
  u <- mem_plan(reg, f)$units[["colapsa"]]
  expect_length(u$invalid, 0L)

  # Agora o caso de verdade: a porta `x` do colapso solta é impossível (ele só
  # entra na região por ela), então o teste usa a porta obrigatória de `junta`.
  g <- tr_flow(reg) |>
    tr_add("fonte", "m/fonte") |>
    tr_add("junta", "m/junta") |>
    tr_link("fonte:out", "junta:a") |>
    tr_add("colapsa", "m/colapsa", from = "junta")
  v <- mem_plan(reg, g)$units[["colapsa"]]
  expect_equal(v$invalid, "missing_required_input:junta:b")
  expect_length(tr_plan_pending(mem_plan(reg, g)), 0L)
})

# --- Alvos, colapsos múltiplos e o resto do plano ----------------------------

test_that("nó interior como alvo resolve a região, não aborta", {
  # `acumula` é TERMINAL: não tem aresta de saída, então `tr_doc_terminals()` o
  # entrega como alvo em todo plano sem alvo explícito. Abortar aqui tornaria
  # um documento saudável improgramável.
  reg <- mem_registry()
  f <- tr_flow(reg) |>
    tr_add("fonte", "m/fonte") |>
    tr_add("colapsa", "m/colapsa", from = "fonte") |>
    tr_add("acumula", "m/acc") |>
    tr_link("fonte:out", "acumula:x")

  p <- mem_plan(reg, f)
  expect_equal(p$targets, c("colapsa", "acumula"))
  expect_length(p$units, 1L)
  expect_equal(names(p$units), "colapsa")
  expect_equal(p$units$colapsa$region$order, c("fonte", "acumula", "colapsa"))

  # Alvo explícito no nó interior: a região é a única forma de computá-lo.
  q <- tr_plan(tr_flow_doc(f), targets = "acumula", registry = reg)
  expect_equal(names(q$units), "colapsa")
  # E pedir a região duas vezes (pelos dois alvos) não a duplica nem sorteia
  # chave nova.
  expect_equal(p$units$colapsa$key, tr_plan(tr_flow_doc(f), targets = c("acumula", "colapsa"),
                                            registry = reg)$units$colapsa$key)
})

test_that("dois colapsos na mesma região: UMA unidade, saídas distintas", {
  reg <- mem_registry()
  f <- tr_flow(reg) |>
    tr_add("fonte", "m/fonte") |>
    tr_add("c1", "m/colapsa", from = "fonte") |>
    tr_add("c2", "m/colapsa") |>
    tr_link("fonte:out", "c2:x") |>
    tr_add("d1", "m/mostra", from = "c1") |>
    tr_add("d2", "m/mostra", from = "c2")

  p <- mem_plan(reg, f)
  # A região roda UMA vez: uma unidade só, nomeada pelo primeiro colapso.
  expect_length(p$units, 3L)
  expect_setequal(names(p$units), c("c1", "d1", "d2"))
  u <- p$units[["c1"]]
  expect_equal(u$region$collapse, c("c1", "c2"))
  # Com mais de um colapso os nomes das saídas são qualificados pelo nó: dois
  # colapsos com uma porta `out` cada colidiriam num mapa por porta, e o
  # segundo artefato ficaria fora do plano — e fora do `keep` do GC.
  expect_setequal(names(u$outputs), c("c1:out", "c2:out"))
  expect_false(identical(u$outputs[["c1:out"]], u$outputs[["c2:out"]]))
  # O mapa que o worker usa pra não ter que redescobrir a regra de nome.
  expect_equal(u$region$outputs$c2[["out"]], "c2:out")
  # Cada consumidor lê a saída do SEU colapso.
  expect_equal(p$units$d1$inputs$x$key, u$outputs[["c1:out"]])
  expect_equal(p$units$d2$inputs$x$key, u$outputs[["c2:out"]])
  # As duas saídas entram no `keep` do GC.
  expect_true(all(unlist(u$outputs) %in% tr_plan_keys(p)))
  # E as duas saem da MESMA chave de unidade.
  expect_equal(u$outputs[["c1:out"]], .tr_out_key(u$key, "c1:out"))
})

test_that("colapso sem porta de saída grava sob a porta vazia", {
  # Um nó que só exibe o histórico é colapso legítimo (come fluxo, não emite
  # fluxo) e não tem porta de saída — a mesma forma que `tr_plan()` já dá a
  # qualquer nó terminal. Sem isto, a região não teria chave nenhuma no `keep`
  # do GC e o run não teria onde marcar "terminou".
  reg <- mem_registry()
  f <- tr_flow(reg) |>
    tr_add("fonte", "m/fonte") |>
    tr_add("c1", "m/colapsa", from = "fonte") |>
    tr_add("ex", "m/exibe") |>
    tr_link("fonte:out", "ex:x")

  u <- mem_plan(reg, f)$units[["c1"]]
  expect_equal(u$region$collapse, c("c1", "ex"))
  # Com dois colapsos o nome é qualificado; o que não tem porta é o nó puro.
  expect_setequal(names(u$outputs), c("c1:out", "ex"))
  expect_equal(names(u$output_types), "c1:out")
  expect_true(all(unlist(u$outputs) %in% tr_plan_keys(mem_plan(reg, f))))
})

test_that("região malformada aborta o plano, alto e cedo", {
  reg <- mem_registry()
  f <- tr_flow(reg) |> tr_add("fonte", "m/fonte") |> tr_add("p", "m/puro", from = "fonte")
  expect_error(mem_plan(reg, f), class = "tr_error_stream_not_collected")
})

test_that("tr_flow_code continua reconstruindo um documento com região", {
  # `tr_flow_code()` percorria `names(plan$units)` pra emitir um `tr_add()` por
  # nó. Com a região virando uma unidade, os membros desapareciam do código
  # gerado — o documento reaberto pelo código teria só o colapso.
  reg <- mem_registry()
  doc <- tr_flow_doc(mem_flow(reg))
  code <- tr_flow_code(doc, registry = reg, registry_expr = "reg")
  novo <- eval(parse(text = code), envir = list2env(list(reg = reg), parent = environment()))
  expect_setequal(names(tr_flow_doc(novo)$nodes), names(doc$nodes))
  expect_equal(tr_plan(tr_flow_doc(novo), registry = reg)$units[["colapsa"]]$key,
               tr_plan(doc, registry = reg)$units[["colapsa"]]$key)
})
