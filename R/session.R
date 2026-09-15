#' Protocolo de op: `{seq, base_rev, op}`.
#'
#' O front emite OPS, nunca o documento inteiro. No insumo o front empurrava o
#' grafo todo com debounce e o servidor diffava pra descobrir se algo relevante
#' tinha mudado — e havia um segundo timer pedindo execução em paralelo, sem
#' garantia de ordem de chegada. Quando a ordem não valia, a engine rodava
#' contra o documento ANTIGO, via que nada mudou, servia do cache, e o preview
#' não atualizava: sem erro, silenciosamente.
#'
#' Aqui essa corrida vira erro explícito: se `base_rev` não bate com a revisão
#' atual, a op é recusada e o cliente ressincroniza com o documento inteiro.
#' Estado nunca fica corrompido em silêncio.
#'
#' `seq` existe por uma armadilha do Shiny: `input$x` ignora valores idênticos
#' consecutivos. Duas ops iguais em sequência (clicar "remover" no mesmo nó
#' duas vezes, mover pro mesmo lugar) desapareceriam sem um campo que muda
#' sempre.
#' @export
tr_submit <- function(doc, envelope, registry = .tr_default_registry) {
  base_rev <- envelope$base_rev
  # `base_rev` AUSENTE é recusa, não isenção. Tratar ausência como "pode
  # aplicar" desliga a proteção inteira em silêncio justamente nos casos em
  # que ela mais importa: cliente com bug, payload truncado, JSON malformado.
  if (length(base_rev) != 1L || !is.numeric(base_rev) || is.na(base_rev)) {
    return(list(ok = FALSE, doc = doc, seq = envelope$seq, reason = "malformed",
                message = "Envelope sem 'base_rev' válido."))
  }
  if (!identical(as.integer(base_rev), doc$rev)) {
    return(list(ok = FALSE, doc = doc, seq = envelope$seq, reason = "stale_rev",
                message = sprintf("Revisão defasada: cliente em %s, servidor em %s.", base_rev, doc$rev)))
  }
  tryCatch({
    res <- .tr_apply(doc, envelope$op, registry)
    # Ecoa a op NORMALIZADA (id/seed materializados, index resolvido), nunca a
    # recebida — é o que faz o log ser replayável. Ver .tr_apply().
    list(ok = TRUE, doc = res$doc, seq = envelope$seq, rev = res$doc$rev,
         op = res$op, semantic = tr_op_semantic(res$op))
  }, error = function(e) {
    list(ok = FALSE, doc = doc, seq = envelope$seq, reason = "rejected",
         message = conditionMessage(e), class = class(e)[1])
  })
}

#' Reaplica um log de ops sobre `doc` — a base do undo.
#'
#' Reexecutar o log é mais simples e mais confiável que inverter cada op
#' (`disconnect` inverso precisaria lembrar o `index`; `remove_node` inverso
#' precisaria lembrar params, seed e posição). O documento é pequeno e as ops
#' são baratas — reconstruir é o caminho honesto.
#'
#' `doc` tem que ser o documento em que o log COMEÇOU. O default vazio só serve
#' pra um log que cobre a história inteira; o undo do editor não é esse caso —
#' ver `.tr_undo_doc()`.
#' @export
tr_replay <- function(ops, registry = .tr_default_registry, doc = tr_doc()) {
  for (op in ops) doc <- tr_doc_apply(doc, op, registry)
  doc
}

#' Undo: o log da sessão reaplicado sobre o documento em que ESSE log começou.
#'
#' O log é do servidor (`tr_server()`) e só cobre as ops aplicadas desde que o
#' front montou; a história anterior (o fluxo aberto do disco, a sessão antes
#' de um reload) não está nele. Reaplicar sobre `tr_doc()` apagava tudo isso:
#' desfazer a primeira op depois de abrir um fluxo de 4 nós replayava `[]`
#' sobre o vazio, e o autosave gravava o fluxo zerado por cima do arquivo. A
#' base é o documento que o servidor mandou ao cliente no `tr_ready` — o mesmo
#' ponto em que o log nasce.
#'
#' A revisão NÃO vem do replay: ela contaria só as ops do log a partir da rev
#' da base, e voltaria (323 → 322). A proteção de `tr_submit()` compara por
#' IGUALDADE com a rev do cliente, e uma rev que já foi emitida antes com outro
#' conteúdo deixaria passar uma op pensada pra um documento que não existe
#' mais. Undo é uma mudança como outra qualquer: avança a revisão atual.
#' @noRd
.tr_undo_doc <- function(base, ops, current_rev, registry = .tr_default_registry) {
  doc <- tr_replay(ops, registry, doc = base)
  doc$rev <- as.integer(current_rev) + 1L
  doc
}
