# A região é DERIVADA do catálogo mais o grafo (Decisão 3 do desenho): nada no
# documento diz "esta aresta transporta fluxo". Então todo teste aqui monta um
# documento normal, pela DSL, e pergunta o que saiu.

regioes <- function(flow) .tr_stream_regions(tr_flow_doc(flow), flow$registry)

# Detecção crua, sem a validação que `.tr_stream_regions()` aplica por cima.
# Os testes que montam PATOLOGIA (duas fontes, região que não fecha, saída
# comum consumida fora) usam esta: a recusa é testada mais abaixo, e aqui o que
# está sob teste é a propagação — que tem que acertar a composição da região
# justamente pra mensagem de erro poder nomear quem está errado.
detecta <- function(flow) .tr_stream_detect(tr_flow_doc(flow), flow$registry)

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

  rs <- detecta(f)
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

  r <- detecta(f)[["fonte"]]
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

  rs <- detecta(f)
  expect_length(rs, 1L)
  r <- rs[["f1"]]
  expect_equal(r$id, "f1")
  expect_equal(r$source, c("f1", "f2"))
  expect_equal(r$nodes, c("f1", "f2", "fim"))
  expect_equal(r$collapse, "fim")
  # A aresta f1 -> f2 é INTERNA: não pode aparecer como entrada de fora. Sai de
  # uma porta DECLARADA como fluxo, e é isso que a distingue do colapso ligado
  # numa fonte, que dá duas regiões — ali a aresta não transporta fluxo.
  expect_length(r$external, 0L)
})

test_that("colapso de uma região que alimenta a fonte da outra dá DUAS regiões", {
  # Documento SAUDÁVEL, e o que o desenho quer: a primeira região colapsa de
  # volta no ecossistema (`c1` grava artefato no store) e a segunda reparte esse
  # artefato em pontos outra vez. Enquanto a componente conexa era calculada com
  # TODA aresta entre membros, `c1:out -> f2:dados` fundia as duas numa região
  # só e o documento era recusado por "mais de uma fonte" — dizendo ao autor que
  # colapsasse um fluxo antes de ligá-lo no outro, que é exatamente o que ele
  # fez. Só aresta que TRANSPORTA fluxo liga componentes.
  f <- tr_flow(stream_registry()) |>
    tr_add("f1", "s/fonte") |>
    tr_add("c1", "s/colapsa", from = "f1") |>
    tr_add("f2", "s/fonte", from = "c1") |>
    tr_add("c2", "s/colapsa", from = "f2")

  rs <- regioes(f)
  expect_length(rs, 2L)
  expect_named(rs, c("f1", "f2"))
  expect_equal(rs[["f1"]]$nodes, c("f1", "c1"))
  expect_equal(rs[["f1"]]$source, "f1")
  expect_equal(rs[["f2"]]$nodes, c("f2", "c2"))
  expect_equal(rs[["f2"]]$source, "f2")

  # E a aresta do colapso pra fonte seguinte NÃO desaparece: ela é entrada
  # comum da segunda região, e é ela que a Fase 3 tem que resolver pra saber em
  # qual porta o artefato de `c1` entra. Classificada como interna, sumia.
  ext <- rs[["f2"]]$external
  expect_length(ext, 1L)
  expect_equal(paste0(ext[[1]]$from$node, ":", ext[[1]]$from$port), "c1:out")
  expect_equal(paste0(ext[[1]]$to$node, ":", ext[[1]]$to$port), "f2:dados")
  # A primeira região não recebe nada de fora.
  expect_length(rs[["f1"]]$external, 0L)
})

test_that("colapso que alimenta porta comum de nó com memória de outra região não funde", {
  # O caso de validação do desenho — "o modelo treinado entra na região por
  # entrada comum" — quando o tal upstream é o colapso de OUTRA região. Mesma
  # fusão indevida, por uma porta comum em vez da porta de dados da fonte.
  f <- tr_flow(stream_registry()) |>
    tr_add("f1", "s/fonte") |>
    tr_add("c1", "s/colapsa", from = "f1") |>
    tr_add("f2", "s/fonte") |>
    tr_add("acc", "s/acumula", from = "f2") |>
    tr_link("c1:out", "acc:k") |>
    tr_add("c2", "s/colapsa", from = "acc")

  rs <- regioes(f)
  expect_length(rs, 2L)
  expect_equal(rs[["f1"]]$nodes, c("f1", "c1"))
  expect_equal(rs[["f2"]]$nodes, c("f2", "acc", "c2"))
  destinos <- vapply(rs[["f2"]]$external, function(e) paste0(e$to$node, ":", e$to$port), "")
  expect_setequal(destinos, "acc:k")
})

test_that("duas regiões alimentando uma terceira: external com dois montantes distintos", {
  # A única forma em que o `external` de UMA região carrega entradas de duas
  # regiões de montante diferentes, e pelas duas formas de entrada ao mesmo
  # tempo: a porta de dados da fonte e a porta comum de um nó da região. É o
  # diamante que a união-busca por aresta de fluxo tem que deixar em paz — três
  # regiões, não uma.
  f <- tr_flow(stream_registry()) |>
    tr_add("f1", "s/fonte") |>
    tr_add("c1", "s/colapsa", from = "f1") |>
    tr_add("f2", "s/fonte") |>
    tr_add("c2", "s/colapsa", from = "f2") |>
    tr_add("f3", "s/fonte", from = "c1") |>
    tr_add("j", "s/acumula", from = "f3") |>
    tr_link("c2:out", "j:k") |>
    tr_add("c3", "s/colapsa", from = "j")

  rs <- regioes(f)
  expect_length(rs, 3L)
  expect_named(rs, c("f1", "f2", "f3"))
  expect_equal(rs[["f3"]]$nodes, c("f3", "j", "c3"))
  origens <- vapply(rs[["f3"]]$external, function(e) paste0(e$from$node, ":", e$from$port), "")
  destinos <- vapply(rs[["f3"]]$external, function(e) paste0(e$to$node, ":", e$to$port), "")
  expect_setequal(origens, c("c1:out", "c2:out"))
  expect_setequal(destinos, c("f3:dados", "j:k"))
})

test_that("saída comum que alimenta a fonte de outra região escapa, não funde", {
  # `fonte:resumo` é saída COMUM de um nó que emite fluxo: não transporta fluxo,
  # logo não liga componentes. Duas regiões — e a recusa certa é `escapes`, que
  # aponta o valor parcial sendo consumido fora, não `multi_source`, que mandaria
  # o autor separar regiões que já estão separadas.
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte_dupla") |>
    tr_add("fim", "s/colapsa") |>
    tr_link("fonte:fluxo", "fim:x") |>
    tr_add("f2", "s/fonte") |>
    tr_link("fonte:resumo", "f2:dados") |>
    tr_add("fim2", "s/colapsa", from = "f2")

  rs <- detecta(f)
  expect_length(rs, 2L)
  expect_named(rs, c("f2", "fonte"))
  expect_equal(rs[["fonte"]]$nodes, c("fonte", "fim"))
  expect_equal(rs[["f2"]]$nodes, c("f2", "fim2"))
  expect_error(regioes(f), class = "tr_error_stream_escapes")
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
  r <- detecta(f)[["fonte"]]
  expect_equal(r$nodes, "fonte")
  expect_equal(r$source, "fonte")
  expect_length(r$collapse, 0L)
})

# --- Validação: as cinco recusas ---------------------------------------------
# Cada uma impede um fluxo que rodaria fazendo OUTRA coisa, calado. Os testes
# de detecção acima chamam `.tr_stream_detect()` justamente porque montam as
# patologias que estas recusas rejeitam.

test_that("duas fontes na mesma região: recusa nas duas formas em que isso acontece", {
  # Forma 1: uma fonte alimentando a entrada comum da outra.
  cascata <- tr_flow(stream_registry()) |>
    tr_add("f1", "s/fonte") |>
    tr_add("f2", "s/fonte") |>
    tr_link("f1:out", "f2:dados") |>
    tr_add("fim", "s/colapsa", from = "f2")
  expect_error(regioes(cascata), class = "tr_error_stream_multi_source")
  # A mensagem nomeia as duas fontes: sem isso, "mais de uma fonte" num
  # documento de 40 nós não diz onde mexer.
  expect_error(regioes(cascata), "f1.*f2")

  # Forma 2: duas fontes que se encontram num nó elevado.
  encontro <- tr_flow(stream_registry()) |>
    tr_add("f1", "s/fonte") |>
    tr_add("f2", "s/fonte") |>
    tr_add("encontro", "s/junta", from = "f1") |>
    tr_link("f2:out", "encontro:b") |>
    tr_add("fim", "s/colapsa", from = "encontro")
  expect_error(regioes(encontro), class = "tr_error_stream_multi_source")
})

test_that("região sem colapso é recusada, nomeando a fonte que não fecha", {
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("meio", "s/puro", from = "fonte")
  expect_error(regioes(f), class = "tr_error_stream_not_collected")
  expect_error(regioes(f), "fonte")
})

test_that("saída comum de nó interior consumida fora da região é recusada", {
  # `s/fonte_dupla` emite pontos por uma porta e um resumo comum pela outra.
  # Dentro da região o resumo é parcial, ponto a ponto: não vira artefato no
  # store. Sem a recusa, `res` falharia com "chave ausente" longe da causa.
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte_dupla") |>
    tr_add("fim", "s/colapsa") |>
    tr_add("res", "s/puro") |>
    tr_link("fonte:fluxo", "fim:x") |>
    tr_link("fonte:resumo", "res:x")

  expect_error(regioes(f), class = "tr_error_stream_escapes")
  expect_error(regioes(f), "res")
})

test_that("nó impuro, volátil ou que pede '.ctx' não pode ser elevado", {
  com_meio <- function(tipo) {
    tr_flow(stream_registry()) |>
      tr_add("fonte", "s/fonte") |>
      tr_add("meio", tipo, from = "fonte") |>
      tr_add("fim", "s/colapsa", from = "meio")
  }

  # Impuro elevado = efeito colateral N mil vezes, um por ponto.
  expect_error(regioes(com_meio("s/impuro")), class = "tr_error_not_liftable")
  expect_error(regioes(com_meio("s/impuro")), "impuro")
  # Volátil elevado nunca reaproveita nada: recomputa a cada ponto.
  expect_error(regioes(com_meio("s/volatil")), class = "tr_error_not_liftable")
  expect_error(regioes(com_meio("s/volatil")), "volátil")
  # `.ctx` pressupõe ser a unidade, não um passo dela.
  expect_error(regioes(com_meio("s/contexto")), class = "tr_error_not_liftable")
  expect_error(regioes(com_meio("s/contexto")), "\\.ctx")

  # O MESMO nó impuro fora do caminho do fluxo não é problema de ninguém: é a
  # prova de que a recusa é da elevação, não do nó.
  fora <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("fim", "s/colapsa", from = "fonte") |>
    tr_add("depois", "s/impuro", from = "fim")
  expect_equal(regioes(fora)[["fonte"]]$nodes, c("fonte", "fim"))
})

test_that("nó com memória impuro e volátil NÃO é recusado: ele roda por declaração", {
  # A isenção do `online` na validação da elevação, que nenhum outro nó da
  # coleção exercita. Um nó com `init`/`step` não é elevado ponto a ponto — o
  # driver chama `step` com o estado — então impureza, volatilidade e `.ctx` não
  # são defeito nele. Sem a isenção, declarar memória num nó que lê o mundo
  # fora do grafo (ler um arquivo a cada ponto é o caso de uso, não o bug)
  # abortaria o documento.
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("acc", "s/acumula_impuro", from = "fonte") |>
    tr_add("fim", "s/colapsa", from = "acc")

  expect_no_error(regioes(f))
  expect_equal(regioes(f)[["fonte"]]$nodes, c("fonte", "acc", "fim"))

  # O contraste: o MESMO defeito num nó SEM memória é recusado. É a prova de que
  # a isenção é do contrato `init`/`step`, não do nó.
  sem_memoria <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("meio", "s/impuro", from = "fonte") |>
    tr_add("fim", "s/colapsa", from = "meio")
  expect_error(regioes(sem_memoria), class = "tr_error_not_liftable")
})

test_that("fonte com nada ligado é recusada por `regioes()`, não só devolvida pela detecção", {
  # O par do teste de detecção lá em cima, contra a entrada pública: a região de
  # um nó só existe no resultado da detecção, mas não sobrevive à validação.
  f <- tr_flow(stream_registry()) |> tr_add("fonte", "s/fonte")
  expect_error(regioes(f), class = "tr_error_stream_not_collected")
  expect_error(regioes(f), "fonte")
})

test_that("com mais de um defeito, a recusa estrutural vem antes da recusa de nó", {
  # A ordem é contrato, não acidente da escrita: defeito da REGIÃO (não tem uma
  # fonte só, não fecha) vem antes de defeito de NÓ (escapa, não é elevável),
  # porque o autor precisa primeiro de uma região bem formada pra que nomear um
  # nó dentro dela signifique algo. Sem este teste, um refactor troca a ordem em
  # silêncio e a primeira mensagem passa a ser a menos útil.

  # Sem uma fonte só E sem colapso: ganha `multi_source`.
  duas_fontes <- tr_flow(stream_registry()) |>
    tr_add("f1", "s/fonte") |>
    tr_add("f2", "s/fonte") |>
    tr_link("f1:out", "f2:dados")
  expect_error(regioes(duas_fontes), class = "tr_error_stream_multi_source")

  # Sem colapso E com nó impuro elevado: ganha `not_collected`.
  sem_fecho <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("meio", "s/impuro", from = "fonte")
  expect_error(regioes(sem_fecho), class = "tr_error_stream_not_collected")

  # Valor parcial escapando E nó impuro elevado: ganha `escapes`.
  escapa <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte_dupla") |>
    tr_add("fim", "s/colapsa") |>
    tr_link("fonte:fluxo", "fim:x") |>
    tr_add("meio", "s/impuro") |>
    tr_link("fonte:fluxo", "meio:x") |>
    tr_add("res", "s/mostra") |>
    tr_link("fonte:resumo", "res:x")
  expect_error(regioes(escapa), class = "tr_error_stream_escapes")
})

test_that("região saudável passa pela validação intacta", {
  # Uma validação que recusa demais é pior que nenhuma: os grafos legítimos da
  # detecção continuam saindo iguais, com nó elevado, nó com memória,
  # ramificação e entrada comum de fora.
  f <- tr_flow(stream_registry()) |>
    tr_add("tab", "s/tabela") |>
    tr_add("fonte", "s/fonte", from = "tab") |>
    tr_add("b", "s/puro", from = "fonte") |>
    tr_add("a", "s/acumula", from = "fonte") |>
    tr_link("tab:out", "a:k") |>
    tr_add("junta", "s/junta", from = "a") |>
    tr_link("b:out", "junta:b") |>
    tr_add("fim", "s/colapsa", from = "junta") |>
    tr_add("depois", "s/mostra", from = "fim")

  rs <- regioes(f)
  expect_length(rs, 1L)
  expect_equal(rs[["fonte"]]$nodes, c("fonte", "a", "b", "junta", "fim"))
  expect_equal(rs[["fonte"]]$collapse, "fim")

  # Duas regiões saudáveis no mesmo documento também passam — a validação é
  # por região, não do documento inteiro.
  duas <- tr_flow(stream_registry()) |>
    tr_add("f1", "s/fonte") |>
    tr_add("fim1", "s/colapsa", from = "f1") |>
    tr_add("f2", "s/fonte") |>
    tr_add("fim2", "s/colapsa", from = "f2")
  expect_named(regioes(duas), c("f1", "f2"))
})

test_that("ponto que chega numa porta comum de nó com memória é permitido", {
  # NÃO é recusa, e é decisão: a aresta de fluxo que cai na porta `k` de um nó
  # com memória é bem definida — é um fluxo só, e o mesmo ponto chega nas duas
  # portas. Recusar isso cortaria um grafo que executa.
  f <- tr_flow(stream_registry()) |>
    tr_add("fonte", "s/fonte") |>
    tr_add("acc", "s/acumula", from = "fonte") |>
    tr_link("fonte:out", "acc:k") |>
    tr_add("fim", "s/colapsa", from = "acc")

  expect_equal(regioes(f)[["fonte"]]$nodes, c("fonte", "acc", "fim"))
})

test_that("nenhum nó de `stream_collection()` tem porta opcional sem default no formal", {
  # A classe de defeito que já quebrou `s/fonte` e `s/fonte_dupla`, e que ficou
  # em `s/acumula`: formal que corresponde a porta `required = FALSE` e não tem
  # default. Porta opcional solta não é passada (é assim que o `fn` cai no
  # próprio default), então o nó aborta "argumento ausente, sem padrão" no
  # instante em que o corpo tocar o argumento. Fica latente enquanto o corpo o
  # ignora, que é exatamente por que os três nasceram juntos e só dois doeram.
  expect_equal(portas_opcionais_sem_default(stream_collection()), character())
})
