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
#' Só DETECÇÃO: nenhuma patologia é recusada aqui. Região sem colapso, ramo que
#' não fecha, duas fontes na mesma região — tudo sai no resultado, porque é
#' exatamente disso que a checagem precisa pra dizer QUAL fonte está aberta.
#' @noRd
.tr_stream_regions <- function(doc, registry) {
  doc <- .tr_as_doc(doc)
  spec_of <- function(id) tr_get_node(doc$nodes[[id]]$type, registry)

  declara_entrada_fluxo <- function(sp) any(vapply(sp$inputs, function(p) isTRUE(p$stream), logical(1)))
  declara_saida_fluxo   <- function(sp) any(vapply(sp$outputs, function(p) isTRUE(p$stream), logical(1)))
  # Colapso: recebe fluxo por declaração e devolve valor comum. É o único nó
  # da região cuja saída tem artefato no store.
  eh_colapso <- function(sp) declara_entrada_fluxo(sp) && !declara_saida_fluxo(sp)

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
  membros <- fontes; colapsos <- character()
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

  .tr_stream_split(doc, membros, fontes, colapsos)
}

#' Parte os nós de fluxo nas componentes conexas ligadas por arestas internas:
#' dois `to_stream` independentes no mesmo documento são duas regiões, que
#' executam separadamente.
#'
#' O `id` da região é o id da fonte — determinístico, legível na mensagem de
#' erro e estável sob renome de qualquer OUTRO nó. Com mais de uma fonte na
#' mesma componente (patologia que a checagem recusa) vale a menor em ordem
#' alfabética, pra mensagem de erro não trocar de nome entre duas chamadas.
#' @noRd
.tr_stream_split <- function(doc, membros, fontes, colapsos) {
  internas <- Filter(function(e) e$from$node %in% membros && e$to$node %in% membros, doc$edges)

  # União-busca sem estrutura dedicada: o rótulo da componente é um vetor
  # nomeado e a união reescreve todos os que tinham o rótulo antigo. Posto e
  # compressão de caminho seriam código a mais sem ganho medível numa região de
  # dezenas de nós.
  comp <- stats::setNames(membros, membros)
  for (e in internas) {
    novo <- comp[[e$from$node]]; velho <- comp[[e$to$node]]
    if (identical(novo, velho)) next
    comp[comp == velho] <- novo
  }

  regioes <- lapply(unique(unname(comp)), function(rotulo) {
    nos <- names(comp)[comp == rotulo]
    fs <- sort(intersect(fontes, nos))
    list(
      id = fs[[1]], source = fs,
      nodes = .tr_stream_topo(nos, Filter(function(e) e$from$node %in% nos, internas)),
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
