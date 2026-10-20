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

#' Um passo de desfazer ou refazer sobre o par (log, refeito).
#'
#' Refazer é o espelho barato do undo por replay: a op desfeita sai do fim do
#' `log` e vai para o topo de `refeito`; refazer a devolve ao log e reaplica.
#' A op é a NORMALIZADA (id/seed materializados), então refazer recria o mesmo
#' nó, não um de id novo. Qualquer op nova zera `refeito` (quem faz isso é o
#' chamador, em `aplicar()`): refazer depois de editar reaplicaria uma op
#' pensada para um documento que já não existe.
#'
#' Devolve `NULL` quando não há o que fazer; erro de replay propaga, e o
#' chamador mantém log e pilha como estavam.
#' @noRd
.tr_historia_passo <- function(base, log, refeito, direcao, current_rev,
                               registry = .tr_default_registry) {
  direcao <- match.arg(direcao, c("desfazer", "refazer"))
  if (is.null(base)) return(NULL)
  if (direcao == "desfazer") {
    if (length(log) == 0L) return(NULL)
    op <- log[[length(log)]]
    log <- log[-length(log)]
    refeito <- c(list(op), refeito)
  } else {
    if (length(refeito) == 0L) return(NULL)
    log <- c(log, refeito[1])
    refeito <- refeito[-1]
  }
  list(doc = .tr_undo_doc(base, log, current_rev, registry), log = log, refeito = refeito)
}

# Documento aberto ----------------------------------------------------------

#' Um documento por projeto e fluxo no PROCESSO, e não um por aba.
#'
#' Cada aba do editor é uma sessão Shiny. Quando cada uma guardava o próprio
#' documento, duas abas no mesmo fluxo divergiam: a edição de uma não chegava
#' à outra, e o autosave da outra regravava o documento velho por cima. O
#' canal de controle piorava isso: ele fala com a última aba aberta, e quando
#' ela fechava a edição seguinte do agente caía na aba antiga, com o documento
#' antigo — um print com Chrome headless apagou assim 7 frames do disco.
#'
#' As abas agora compartilham o documento E o histórico de desfazer (`base`,
#' `log`, `refeito`): um fluxo, uma história, e o Ctrl+Z de qualquer aba
#' desfaz a última op, venha de onde vier. Quem muda o documento passa por
#' `.tr_aberto_publicar()`, que entrega o novo às outras abas. A entrada nasce
#' do disco quando a primeira aba entra e some quando a última sai: a próxima
#' lê o disco de novo, e o que foi mudado por fora com o editor fechado vale.
#' @noRd
.tr_abertos <- new.env(parent = emptyenv())

.tr_aberto_chave <- function(raiz, fluxo) {
  paste0(normalizePath(raiz, mustWork = FALSE), "\n", fluxo)
}

#' Entra no documento aberto de `fluxo` em `projeto`, criando-o do disco se
#' nenhuma aba o tem. `receber(doc, semantico)` é como esta aba fica sabendo
#' das mudanças feitas pelas outras.
#' @noRd
.tr_aberto_entrar <- function(projeto, fluxo, token, receber) {
  chave <- .tr_aberto_chave(projeto$root, fluxo)
  a <- .tr_abertos[[chave]]
  if (is.null(a)) {
    a <- new.env(parent = emptyenv())
    a$chave <- chave
    a$membros <- list()
    .tr_aberto_recomecar(a, tr_project_flow(projeto, fluxo))
    assign(chave, a, envir = .tr_abertos)
  }
  a$membros[[token]] <- receber
  a
}

#' Documento novo e história vazia: reaplicar o log antigo sobre um documento
#' relido do disco é o que `.tr_undo_doc()` existe para impedir.
#' @noRd
.tr_aberto_recomecar <- function(a, doc) {
  a$doc <- doc; a$base <- doc; a$log <- list(); a$refeito <- list()
  invisible(a)
}

.tr_aberto_sair <- function(a, token) {
  a$membros[[token]] <- NULL
  if (!length(a$membros) && identical(.tr_abertos[[a$chave]], a)) {
    rm(list = a$chave, envir = .tr_abertos)
  }
  invisible()
}

#' Troca o documento e avisa as outras abas. Uma aba que falha ao receber não
#' pode impedir a edição de quem publicou: o documento já mudou, e é ele que o
#' autosave grava.
#' @noRd
.tr_aberto_publicar <- function(a, doc, token, semantico = TRUE) {
  a$doc <- doc
  for (t in setdiff(names(a$membros), token)) {
    tryCatch(a$membros[[t]](doc, semantico),
             error = function(e) message("trama: aba não recebeu o documento: ", conditionMessage(e)))
  }
  invisible(doc)
}
