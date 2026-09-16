# A região é DERIVADA do catálogo mais o grafo (Decisão 3 do desenho): nada no
# documento diz "esta aresta transporta fluxo". Então todo teste aqui monta um
# documento normal, pela DSL, e pergunta o que saiu.

regioes <- function(flow) .tr_stream_regions(tr_flow_doc(flow), flow$registry)

test_that("fonte -> colapsa: uma região de dois nós, com fonte e colapso identificados", {
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("fim", "s/colapsa", from = "fonte")

  rs <- regioes(f)
  expect_length(rs, 1L)
  expect_named(rs, "fonte")
  r <- rs[["fonte"]]
  expect_equal(r$id, "fonte")
  expect_equal(r$source, "fonte")
  expect_equal(r$collapse, "fim")
  expect_equal(r$nodes, c("fonte", "fim"))
  expect_length(r$external, 0L)
})

test_that("o nó puro é elevado: entra na região sem declarar fluxo nenhum", {
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("meio", "s/puro", from = "fonte") |>
    tr_add("fim", "s/colapsa", from = "meio")

  r <- regioes(f)[["fonte"]]
  expect_equal(r$nodes, c("fonte", "meio", "fim"))
  expect_equal(r$collapse, "fim")
  # A prova de que a elevação é derivada: o MESMO nó, fora do caminho do
  # fluxo, não vira membro de nada.
  fora <- tr_flow(stream_registry()) |>
    tr_add("tab", "s/tabela") |>
    tr_add("meio", "s/puro", from = "tab")
  expect_length(regioes(fora), 0L)
})

test_that("nó com memória entra na região e continua emitindo fluxo", {
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("acc", "s/acumula", from = "fonte") |>
    tr_add("fim", "s/colapsa", from = "acc")

  r <- regioes(f)[["fonte"]]
  expect_equal(r$nodes, c("fonte", "acc", "fim"))
  expect_equal(r$collapse, "fim")
})

test_that("ramificação: os dois ramos caem na mesma região, em ordem topológica", {
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("b", "s/puro", from = "fonte") |>
    tr_add("a", "s/acumula", from = "fonte") |>
    tr_add("junta", "s/junta", from = "a") |>
    tr_link("b:out", "junta:b") |>
    tr_add("fim", "s/colapsa", from = "junta")

  rs <- regioes(f)
  expect_length(rs, 1L)
  r <- rs[["fonte"]]
  # `a` e `b` empatam em profundidade: a ordem é alfabética, não a de inserção
  # (aqui `b` entrou primeiro). Sem critério de empate, dois documentos iguais
  # montados em ordens diferentes dariam regiões diferentes — e a chave da
  # região, que é o fingerprint de todos os nós dela, mudaria sozinha.
  expect_equal(r$nodes, c("fonte", "a", "b", "junta", "fim"))
  expect_equal(r$collapse, "fim")
})

test_that("diamante com ramos de comprimentos diferentes não duplica ninguém", {
  # `junta` é alcançado por um caminho de 1 aresta e por outro de 2: é o caso
  # que a propagação por camadas descobria duas vezes no mesmo passo.
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("longo", "s/puro", from = "fonte") |>
    tr_add("junta", "s/junta", from = "fonte") |>
    tr_link("longo:out", "junta:b") |>
    tr_add("fim", "s/colapsa", from = "junta")

  r <- regioes(f)[["fonte"]]
  expect_equal(r$collapse, "fim")
  expect_equal(r$nodes, c("fonte", "longo", "junta", "fim"))
  # O nó do encontro aparece UMA vez, mesmo alcançado por dois caminhos.
  expect_equal(sum(r$nodes == "junta"), 1L)
})

test_that("duas fontes independentes dão duas regiões distintas", {
  f <- tr_flow(stream_registry()) |>
    tr_add("f2", "s/fonte") |>
    tr_add("fim2", "s/colapsa", from = "f2") |>
    tr_add("f1", "s/fonte") |>
    tr_add("fim1", "s/colapsa", from = "f1")

  rs <- regioes(f)
  expect_length(rs, 2L)
  # Ordenadas pelo id da região (o da fonte), não pela ordem de inserção.
  expect_named(rs, c("f1", "f2"))
  expect_equal(rs[["f1"]]$nodes, c("f1", "fim1"))
  expect_equal(rs[["f2"]]$nodes, c("f2", "fim2"))
})

test_that("duas fontes que se encontram num nó elevado são UMA região", {
  # Também patologia — mas a componente conexa é o critério, não a fonte: se
  # cada fonte virasse uma região, o nó do encontro apareceria nas duas e
  # executaria duas vezes, com dois relógios de ponto diferentes.
  f <- tr_flow(stream_registry()) |>
    tr_add("f1", "s/fonte") |>
    tr_add("f2", "s/fonte") |>
    tr_add("encontro", "s/junta", from = "f1") |>
    tr_link("f2:out", "encontro:b") |>
    tr_add("fim", "s/colapsa", from = "encontro")

  rs <- regioes(f)
  expect_length(rs, 1L)
  expect_named(rs, "f1")
  expect_equal(rs[["f1"]]$source, c("f1", "f2"))
  expect_equal(rs[["f1"]]$nodes, c("f1", "f2", "encontro", "fim"))
})

test_that("nó fora do caminho do fluxo não entra em região nenhuma", {
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("fim", "s/colapsa", from = "fonte") |>
    tr_add("depois", "s/mostra", from = "fim") |>
    tr_add("solto", "s/tabela") |>
    tr_add("solto2", "s/puro", from = "solto")

  r <- regioes(f)[["fonte"]]
  # `depois` fica FORA: o colapso devolve valor comum, e a jusante dele é
  # grafo normal. Se entrasse, um nó de gráfico seria elevado ponto a ponto.
  expect_equal(r$nodes, c("fonte", "fim"))
  expect_false(any(c("depois", "solto", "solto2") %in% r$nodes))
})

test_that("num nó que declara fluxo, só a saída declarada propaga", {
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte_dupla") |>
    tr_add("fim", "s/colapsa") |>
    tr_add("res", "s/puro") |>
    tr_link("fonte:fluxo", "fim:x") |>
    tr_link("fonte:resumo", "res:x")

  r <- regioes(f)[["fonte"]]
  # `res` come do MESMO nó, por uma saída comum: recebe tabela, não pontos.
  # Sem a checagem por porta, toda saída de quem emite arrastaria o consumidor
  # pra dentro da região e ele seria elevado por engano.
  expect_equal(r$nodes, c("fonte", "fim"))
  expect_false("res" %in% r$nodes)
})

test_that("fonte que alimenta outra fonte não reprocessa nem duplica ninguém", {
  # Patologia (duas fontes na mesma região — a validação vai recusar), mas é o
  # caso que fazia a propagação por camadas reemitir um nó: a fonte alimentada
  # está na fila inicial E é descoberta como membro no mesmo passo.
  f <- tr_flow(stream_registry()) |>
    tr_add("f1", "s/fonte") |>
    tr_add("f2", "s/fonte") |>
    tr_link("f1:out", "f2:dados") |>
    tr_add("fim", "s/colapsa", from = "f2")

  rs <- regioes(f)
  expect_length(rs, 1L)
  r <- rs[["f1"]]
  expect_equal(r$id, "f1")
  expect_equal(r$source, c("f1", "f2"))
  expect_equal(r$nodes, c("f1", "f2", "fim"))
  expect_equal(r$collapse, "fim")
  # A aresta f1 -> f2 é INTERNA: não pode aparecer como entrada de fora.
  expect_length(r$external, 0L)
})

test_that("aresta que entra de fora da região vira `external`", {
  f <- tr_flow(stream_registry()) |>
    tr_add("tab", "s/tabela") |>
    tr_add("fonte", "s/fonte", from = "tab") |>
    tr_add("modelo", "s/tabela") |>
    tr_add("acc", "s/acumula", from = "fonte") |>
    tr_link("modelo:out", "acc:k") |>
    tr_add("fim", "s/colapsa", from = "acc")

  r <- regioes(f)[["fonte"]]
  expect_equal(r$nodes, c("fonte", "acc", "fim"))
  # As duas arestas comuns que alimentam a região de fora — a tabela da fonte
  # e a constante do nó com memória (o "modelo treinado" do desenho). Viram as
  # entradas comuns da unidade-região na Fase 3.
  destinos <- vapply(r$external, function(e) paste0(e$to$node, ":", e$to$port), "")
  origens <- vapply(r$external, function(e) e$from$node, "")
  expect_setequal(destinos, c("fonte:dados", "acc:k"))
  expect_setequal(origens, c("tab", "modelo"))
  # `tab` e `modelo` alimentam a região mas NÃO são membros dela.
  expect_false(any(c("tab", "modelo") %in% r$nodes))
})

test_that("documento sem fonte nenhuma não tem região", {
  f <- tr_flow(stream_registry()) |>
    tr_add("tab", "s/tabela") |>
    tr_add("puro", "s/puro", from = "tab")
  expect_identical(regioes(f), list())

  # Documento vazio também.
  expect_identical(regioes(tr_flow(stream_registry())), list())
})

test_that("fonte com nada ligado ainda é uma região de um nó", {
  # Detecção, não validação: a região sem colapso é DEVOLVIDA, e recusá-la é
  # trabalho da checagem — que precisa desta região em mãos pra dizer qual
  # fonte não fecha.
  f <- tr_flow(stream_registry()) |> tr_add("fonte", "s/fonte")
  r <- regioes(f)[["fonte"]]
  expect_equal(r$nodes, "fonte")
  expect_equal(r$source, "fonte")
  expect_length(r$collapse, 0L)
})
