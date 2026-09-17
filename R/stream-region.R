#' Regiões de fluxo de um documento — o fecho transitivo a partir das portas
#' de fluxo.
#'
#' A "streaminess" de uma ARESTA é derivada, nunca declarada no documento (ver
#' Decisão 3 do desenho). Tem que ser derivada: um nó puro elevado
#' (`data/filter`) não declara nada e recebe ponto dentro da região e tabela
#' fora dela — a mesma aresta, o mesmo documento, dois modos. Guardar isso na
#' aresta criaria uma segunda verdade que pode divergir do catálogo.
#'
#' Duas regras, e é só isso:
#'   propaga  — nó que recebe fluxo emite fluxo, A MENOS QUE declare entrada
#'              de fluxo e nenhuma saída de fluxo (esse é o colapso).
#'   pertence — nó com ao menos uma aresta de entrada que transporta fluxo,
#'              mais a própria fonte.
#'
#' Detecta E valida: toda região devolvida por aqui tem execução definida. A
#' recusa mora na mesma chamada porque `tr_plan()` vai chamar esta função na
#' Fase 3 — grafo de fluxo malformado aborta o planejamento, alto e cedo, como
#' `tr_error_cycle` e `tr_error_unknown_node` já fazem lá.
#' @noRd
.tr_stream_regions <- function(doc, registry) {
  doc <- .tr_as_doc(doc)
  regioes <- .tr_stream_detect(doc, registry)
  for (r in regioes) .tr_stream_validate(r, doc, registry)
  regioes
}

#' Só DETECÇÃO: nenhuma patologia é recusada aqui. Região sem colapso, ramo que
#' não fecha, duas fontes na mesma região — tudo sai no resultado, porque é
#' exatamente disso que a validação precisa pra dizer QUAL fonte está aberta.
#' @noRd
.tr_stream_detect <- function(doc, registry) {
  doc <- .tr_as_doc(doc)
  spec_of <- function(id) tr_get_node(doc$nodes[[id]]$type, registry)

  declara_entrada_fluxo <- function(sp) any(vapply(sp$inputs, function(p) isTRUE(p$stream), logical(1)))
  declara_saida_fluxo   <- function(sp) any(vapply(sp$outputs, function(p) isTRUE(p$stream), logical(1)))
  # Colapso: recebe fluxo por declaração e devolve valor comum. É o único nó
  # da região cuja saída tem artefato no store.
  # `!online` faz parte do predicado por duas razões, e a segunda foi MEDIDA
  # depois de eu a ter escrito como se fosse só elegância.
  #
  # A primeira: um colapso roda `fn` UMA vez depois do laço, um nó com memória
  # roda `step` a cada passo. São coisas distintas por construção, e a detecção
  # tem que dizer isso em vez de acertar por acidente de declaração.
  #
  # A segunda, e é ela que torna esta linha indispensável: `tr_node()` recusa nó
  # com memória que tenha saída e nenhuma seja fluxo, mas PERMITE de propósito o
  # nó com memória sem porta de saída nenhuma (terminal que só acumula e publica
  # parcial — a forma de um detector de drift com contador). Nessa forma,
  # `declara_saida_fluxo()` avalia `any(logical(0))`, que é FALSE, e a entrada de
  # fluxo é TRUE: sem o `!online` o predicado classificaria justamente o caso
  # permitido como o colapso da região. Ou seja, as duas guardas são
  # complementares, e não cinto-e-suspensório: `tr_node()` fecha "tem saída,
  # nenhuma é fluxo" e esta fecha "não tem saída".
  eh_colapso <- function(sp) !isTRUE(sp$online) &&
    declara_entrada_fluxo(sp) && !declara_saida_fluxo(sp)

  fontes <- Filter(function(id) declara_saida_fluxo(spec_of(id)) && !declara_entrada_fluxo(spec_of(id)),
                   names(doc$nodes))
  if (length(fontes) == 0) return(list())

  # Índice por ORIGEM — o simétrico do `by_target` de `tr_plan()`, porque a
  # propagação anda no sentido das arestas.
  by_source <- list()
  for (e in doc$edges) by_source[[e$from$node]] <- c(by_source[[e$from$node]], list(e))

  # Fila de EMISSORES, não camadas de largura. A versão por camadas dependia de
  # `unique()` pra não contar duas vezes o nó achado por dois ramos no mesmo
  # passo (o diamante), e reemitia a fonte que outra fonte alimenta — ela está
  # na fila inicial E é descoberta como membro. Com a fila, cada nó emite no
  # máximo uma vez e a corretude não depende de deduplicar nada depois.
  membros <- fontes; colapsos <- character(); arestas_fluxo <- list()
  fila <- fontes; emitiu <- character()
  while (length(fila) > 0) {
    atual <- fila[[1]]; fila <- fila[-1]
    if (atual %in% emitiu) next
    emitiu <- c(emitiu, atual)
    sp_from <- spec_of(atual)
    for (e in by_source[[atual]] %||% list()) {
      # Só a porta DECLARADA como fluxo propaga, na fonte e no nó com memória;
      # num nó elevado não há declaração, e todas as saídas propagam.
      if (declara_saida_fluxo(sp_from) && !isTRUE(sp_from$outputs[[e$from$port]]$stream)) next
      # Chegou aqui: esta aresta TRANSPORTA fluxo. Anotar acontece neste ponto
      # porque é aqui que o predicado mora, e é este conjunto — não "toda aresta
      # entre membros" — que liga as componentes em `.tr_stream_split()`. Com
      # todas as arestas, o colapso de uma região ligado na fonte da outra (uma
      # região colapsa de volta no ecossistema, a seguinte reparte o artefato)
      # fundia as duas numa componente só: documento saudável recusado por "mais
      # de uma fonte", mandando o autor fazer o que ele já tinha feito.
      # Saída de colapso não entra aqui de graça: colapso nunca vai pra fila,
      # então este laço nunca percorre as saídas dele.
      arestas_fluxo <- c(arestas_fluxo, list(e))
      alvo <- e$to$node
      if (!(alvo %in% membros)) membros <- c(membros, alvo)
      if (eh_colapso(spec_of(alvo))) {
        # O colapso entra na região e PARA a propagação: a jusante dele é grafo
        # comum. Sem isso, um nó de gráfico ligado ao histórico seria elevado
        # ponto a ponto.
        colapsos <- union(colapsos, alvo)
      } else {
        fila <- c(fila, alvo)
      }
    }
  }

  .tr_stream_split(doc, membros, fontes, colapsos, arestas_fluxo)
}

#' A PRIMEIRA recusa que uma região viola, ou `NULL` se ela tem execução
#' definida. Cinco recusas, e cada uma impede um fluxo que rodaria fazendo
#' OUTRA coisa, calado.
#'
#' Única fonte de verdade das cinco recusas — `.tr_stream_validate()` (aborta,
#' backstop de `tr_plan()`) e `.tr_stream_problems()` (devolve como `problems`
#' de `tr_doc_validate()`, Decisão 7 do desenho) chamam ESTA função e só
#' escolhem o que fazer com o resultado. Duplicar as cinco condições em dois
#' lugares é como a mensagem de uma diverge silenciosamente da outra — este
#' arquivo já tem um comentário longo (`.tr_stream_detect()`, acima) sobre o
#' preço de duas verdades que podem divergir; aqui a mesma lição vale para a
#' RECUSA, não só a detecção.
#'
#' A mensagem sempre nomeia o nó (ou a região) e diz o que fazer: quem lê é o
#' autor do documento no editor, não quem escreveu este arquivo.
#'
#' A ORDEM das recusas é parte do contrato (e tem teste): defeito ESTRUTURAL da
#' região — não tem uma fonte só, não fecha — vem antes de defeito de NÓ —
#' escapa, não é elevável — porque o autor precisa primeiro de uma região bem
#' formada pra que nomear um nó dentro dela signifique alguma coisa. Por isso
#' esta função devolve só a PRIMEIRA violação: nem `tr_plan()` aborta com mais
#' de uma causa ao mesmo tempo, nem o card mostra duas explicações que talvez
#' se contradigam (ex.: "não fecha" E "nó X escapa" quando o nó X só escapa
#' PORQUE a região não fechou).
#' @noRd
.tr_stream_region_problem <- function(region, doc, registry) {
  spec_of <- function(id) tr_get_node(doc$nodes[[id]]$type, registry)

  # Duas fontes na mesma região é lockstep de dois fluxos de tamanhos
  # diferentes, e isso não tem semântica definida. Sem recusar, o driver
  # andaria com um dos dois e o outro seria truncado ou reciclado em silêncio —
  # a decisão de QUAL ficaria escondida na ordem das arestas. YAGNI: uma fonte
  # por região.
  if (length(region$source) > 1) {
    return(list(
      class = "tr_error_stream_multi_source", node = region$id,
      message = sprintf(
        paste0("A região de fluxo '%s' tem mais de uma fonte: %s. ",
               "Uma região executa uma fonte só — separe os fluxos em regiões ",
               "independentes, ou colapse um antes de ligá-lo no outro."),
        region$id, paste(sprintf("'%s'", region$source), collapse = ", "))))
  }

  # Região sem colapso não produz artefato nenhum: o run não teria o que
  # gravar e o nó nunca sairia de "computando…", porque não existe ponto em que
  # ele termine.
  if (length(region$collapse) == 0) {
    return(list(
      class = "tr_error_stream_not_collected", node = region$id,
      message = sprintf(
        paste0("A região de fluxo de '%s' não fecha: nenhum nó colapsa o fluxo num valor. ",
               "Ligue a ponta da região num nó que receba fluxo e devolva valor comum."),
        region$id)))
  }

  # Saída COMUM de nó interior consumida fora da região. Pelas regras de
  # propagação, quem come fluxo entra na região — e entra na MESMA componente,
  # porque é a aresta de fluxo que liga componentes. Então o único jeito de
  # escapar é uma porta não declarada como fluxo num nó que também emite fluxo;
  # porta de fluxo apontando pra fora daqui não existe, e não há o que testar.
  # Dentro da região essa porta só tem o valor parcial do ponto da vez, e nada
  # dela vai pro store (só o colapso vai): sem recusar, o consumidor de fora
  # falharia com "chave ausente", longe da causa.
  for (e in doc$edges) {
    if (!(e$from$node %in% region$nodes) || e$to$node %in% region$nodes) next
    if (e$from$node %in% region$collapse) next
    return(list(
      class = "tr_error_stream_escapes", node = e$from$node,
      message = sprintf(
        paste0("A porta '%s:%s' está dentro da região de fluxo '%s' mas alimenta '%s', ",
               "fora dela. Dentro da região ela só tem o valor parcial do ponto da vez, ",
               "e não grava artefato: '%s' não teria o que ler. ",
               "Consuma esse valor depois do colapso, ou traga '%s' para dentro da região."),
        e$from$node, e$from$port, region$id, e$to$node, e$to$node, e$to$node)))
  }

  # Nó elevado ponto a ponto: nem fonte, nem colapso, nem nó com memória (esse
  # tem contrato próprio, `init`/`step`, e roda por declaração). O que sobra é
  # o nó comum, que a região passa a chamar uma vez por ponto.
  for (id in setdiff(region$nodes, c(region$source, region$collapse))) {
    spec <- spec_of(id)
    if (isTRUE(spec$online)) next
    motivo <- if (!isTRUE(spec$pure)) {
      # Impuro elevado = o efeito colateral (ler arquivo, chamar API) acontece
      # N mil vezes, uma por ponto, em vez de uma por execução.
      "é impuro"
    } else if (isTRUE(spec$volatile)) {
      # Volátil nunca reaproveita resultado: recomputa do zero a cada ponto, e
      # a região inteira reexecutaria a cada run sem nunca acertar o cache.
      "é volátil"
    } else if (".ctx" %in% names(formals(spec$fn))) {
      # `.ctx` é o contexto da UNIDADE (raiz do projeto, store, run). Pedir ele
      # pressupõe ser a unidade, não um passo dentro dela.
      "pede '.ctx'"
    } else {
      next
    }
    return(list(
      class = "tr_error_not_liftable", node = id,
      message = sprintf(
        paste0("Nó '%s' está dentro de uma região de fluxo mas %s. ",
               "Nós elevados ponto a ponto precisam ser puros e não pedir '.ctx'. ",
               "Se ele precisa guardar estado entre pontos, declare 'init'/'step'."),
        id, motivo)))
  }

  NULL
}

#' Recusa toda região que não tem execução definida — backstop de `tr_plan()`.
#' Aborta com a PRIMEIRA violação (`.tr_stream_region_problem()`); um grafo
#' malformado nunca deveria chegar aqui, porque `tr_doc_validate()` já mostrou
#' a mesma recusa como `problem` no card, alto e cedo (Decisão 7) — mas é o
#' backstop, e continua sendo a autoridade final.
#' @noRd
.tr_stream_validate <- function(region, doc, registry) {
  p <- .tr_stream_region_problem(region, doc, registry)
  if (!is.null(p)) rlang::abort(p$message, class = p$class)
  invisible(region)
}

#' As cinco recusas de região, como `problems` — não como abort. É o que
#' `tr_doc_validate()` chama (Decisão 7 do desenho: "erro na validação de
#' aresta, alto e cedo"). Só um GRAFO WALK: `.tr_stream_detect()` (a metade
#' NÃO-abortante da detecção, existente desde a Fase 2 pra justamente testar
#' região patológica sem exceção) mais `.tr_stream_region_problem()` por
#' região — nenhum fingerprint, nenhum disco, e roda em TODO `tr_doc_validate()`
#' (cada op), então o custo tem que ficar aqui.
#'
#' Uma `problem` por região malformada (a primeira violação, mesma ordem do
#' backstop) — não uma por violação: duas explicações que podem se contradizer
#' (ver comentário de `.tr_stream_region_problem()`) seriam pior que uma.
#' @noRd
.tr_stream_problems <- function(doc, registry) {
  regioes <- .tr_stream_detect(doc, registry)
  problems <- list()
  for (r in regioes) {
    p <- .tr_stream_region_problem(r, doc, registry)
    if (is.null(p)) next
    problems[[length(problems) + 1L]] <- list(
      kind = sub("^tr_error_", "", p$class), node = p$node, region = r$id, message = p$message)
  }
  problems
}

#' Parte os nós de fluxo nas componentes conexas ligadas pelas arestas que
#' TRANSPORTAM fluxo (`fluxo`, anotado na propagação): dois `to_stream`
#' independentes no mesmo documento são duas regiões, que executam
#' separadamente.
#'
#' Só as arestas de fluxo ligam componentes. Uma aresta comum entre dois
#' membros de regiões DIFERENTES — o colapso de uma alimentando a fonte ou a
#' entrada comum da outra — não é ligação: é entrada de fora, e tem que sair em
#' `external` pra Fase 3 resolver. Ligar por ela fundia as duas regiões e
#' recusava um documento saudável por "mais de uma fonte".
#'
#' O `id` da região é o id da fonte — determinístico, legível na mensagem de
#' erro e estável sob renome de qualquer OUTRO nó. Com mais de uma fonte na
#' mesma componente (patologia que a checagem recusa) vale a menor em ordem
#' alfabética, pra mensagem de erro não trocar de nome entre duas chamadas.
#' @noRd
.tr_stream_split <- function(doc, membros, fontes, colapsos, fluxo) {
  # União-busca sem estrutura dedicada: o rótulo da componente é um vetor
  # nomeado e a união reescreve todos os que tinham o rótulo antigo. Posto e
  # compressão de caminho seriam código a mais sem ganho medível numa região de
  # dezenas de nós.
  comp <- stats::setNames(membros, membros)
  for (e in fluxo) {
    novo <- comp[[e$from$node]]; velho <- comp[[e$to$node]]
    if (identical(novo, velho)) next
    comp[comp == velho] <- novo
  }

  regioes <- lapply(unique(unname(comp)), function(rotulo) {
    nos <- names(comp)[comp == rotulo]
    fs <- sort(intersect(fontes, nos))
    list(
      id = fs[[1]], source = fs,
      # A ordem topológica usa TODA aresta interna à região, não só as de
      # fluxo: uma aresta comum entre dois nós da MESMA região é dependência de
      # dados de verdade, e quem a ignora roda o consumidor antes do produtor.
      # O `to %in% nos` é higiene, não necessidade: `.tr_stream_topo()` só lê
      # `preds` de quem está na região, então aresta que sai para outra região
      # seria inerte. Medido, não suposto — afrouxar a condição mantém a suíte
      # verde e a ordem correta.
      nodes = .tr_stream_topo(nos, Filter(function(e) e$from$node %in% nos && e$to$node %in% nos,
                                          doc$edges)),
      collapse = sort(intersect(colapsos, nos)),
      # As entradas comuns da unidade-região na Fase 3: o modelo treinado, a
      # tabela que a fonte reparte. Guardamos a ARESTA inteira porque é ela que
      # diz em qual porta da região o valor de fora entra.
      external = Filter(function(e) e$to$node %in% nos && !(e$from$node %in% nos), doc$edges)
    )
  })
  regioes <- regioes[order(vapply(regioes, function(r) r$id, ""))]
  stats::setNames(regioes, vapply(regioes, function(r) r$id, ""))
}

#' Ordem topológica dos nós da região, pelas arestas internas — o driver vai
#' caminhar nela ponto a ponto, e um nó elevado só roda depois de quem o
#' alimenta.
#'
#' Kahn por camadas, com empate resolvido em ordem alfabética. Sem critério de
#' empate a ordem viria da inserção das arestas: dois documentos iguais montados
#' em ordens diferentes dariam regiões diferentes, e a chave da região (que é o
#' fingerprint de todos os nós dela) mudaria sozinha.
#'
#' `tr_plan()` também devolve ordem topológica, mas não serve aqui: ele calcula
#' impressão digital e chave de conteúdo do fecho inteiro do alvo — incluindo
#' `fingerprint()` que vai ao disco — trabalho que a detecção da região não tem
#' o que fazer com.
#' @noRd
.tr_stream_topo <- function(nos, internas) {
  preds <- lapply(stats::setNames(nos, nos), function(id) character())
  for (e in internas) preds[[e$to$node]] <- union(preds[[e$to$node]], e$from$node)

  ordem <- character(); restante <- sort(nos)
  while (length(restante) > 0) {
    prontos <- Filter(function(id) length(setdiff(preds[[id]], ordem)) == 0, restante)
    # Ciclo dentro da região é impossível por aqui — `tr_doc_apply()` recusa na
    # edição e `.tr_find_cycle()` na leitura. Sair com o que sobrou, em ordem
    # alfabética, é melhor que laço infinito num invariante quebrado.
    if (length(prontos) == 0) return(c(ordem, restante))
    ordem <- c(ordem, prontos)
    restante <- setdiff(restante, prontos)
  }
  ordem
}
