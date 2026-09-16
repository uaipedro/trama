#' Coordenador: monta o plano, despacha o que falta, emite eventos.
#'
#' Pull-based herdado do insumo — nó fora do caminho do alvo não roda. O
#' "acender os cards em sequência" continua saindo de graça da ordem real de
#' execução: o coordenador não calcula fila nenhuma, só emite o que acontece.
#'
#' `on_event(ev)` recebe, por unidade: `cached`, `failed`, `invalid`,
#' `blocked`, `running`, `done`; e um `run_finished` no fim. É o contrato que
#' o transporte (E5) traduz pra mensagem de front — o motor não sabe o que é
#' Shiny.
#' @export
tr_run <- function(doc, targets = NULL, registry = .tr_default_registry, store,
                   executor = tr_executor_sequential(),
                   on_event = function(ev) invisible(NULL), run_id = NULL, settings = NULL) {
  doc <- .tr_as_doc(doc)
  plan <- tr_plan(doc, targets, registry, store, settings = settings)
  tr_run_plan(plan, registry, store, executor, on_event, run_id)
}

#' Executa um plano já montado — separado de `tr_run()` porque o coordenador
#' interativo (E5) monta o plano a cada edição pra decidir o que cancelar, e
#' só então manda executar.
#' @export
tr_run_plan <- function(plan, registry = .tr_default_registry, store,
                        executor = tr_executor_sequential(),
                        on_event = function(ev) invisible(NULL), run_id = NULL) {
  s <- tr_scheduler(plan, registry, store, executor, on_event, run_id)
  # Headless: bombeia até acabar. Dorme só quando há algo em voo e nada
  # progrediu — o executor sequencial termina dentro de `submit`, então nunca
  # dorme; o pool dorme 5ms entre coletas.
  while (!s$step()) if (s$inflight() > 0) Sys.sleep(0.005)
  invisible(s$result())
}

.tr_upstream_nodes <- function(unit) {
  unlist(lapply(unit$inputs, function(i) {
    if (is.null(i)) NULL else if (is.null(i$key)) vapply(i, function(r) r$node, "") else i$node
  }))
}

#' Valor de qualquer nó do grafo, no console.
#'
#' O caminho inverso do nível 1 (`tr_fn(id)`), e a outra metade do que impede
#' a ferramenta de virar gaiola: dá pra inspecionar no REPL o que qualquer
#' caixa da tela produziu, sem passar pela UI.
#'
#' Reaproveita o plano que `tr_run()` já devolve, em vez de montar outro. Um
#' segundo plano sorteia um `nonce` novo pra nó `volatile`, e a chave
#' consultada não seria a que acabou de ser gravada.
#'
#' `settings` é o `project$settings`: tema entra no hash, e sem ele o console
#' resolveria o tema pelos embutidos e calcularia chave diferente da sessão —
#' recomputando o que já está no cache, ou pior, mostrando outro gráfico.
#' @export
tr_value <- function(doc, node, registry = .tr_default_registry, store,
                     port = NULL, executor = tr_executor_sequential(), settings = NULL) {
  res <- tr_run(doc, targets = node, registry = registry, store = store, executor = executor,
                settings = settings)
  u <- res$plan$units[[node]]
  # Membro INTERIOR de região não tem unidade própria: a região é uma unidade
  # só, nomeada pelo colapso. Sem este ramo `u` era NULL,
  # `length(u$output_types)` dava 0, e o `sprintf` com `u$node_type = NULL`
  # devolvia `character(0)` — o abort saía com a mensagem VAZIA, e quem pedisse
  # o valor de um nó de dentro da região não recebia pista nenhuma. É pra isto
  # que `plan$region_of` existe; era o único lugar do pacote que devia lê-lo.
  if (is.null(u)) {
    rid <- res$plan$region_of[[node]]
    if (!is.null(rid)) {
      rlang::abort(sprintf(
        paste0("'%s' é nó interior da região de fluxo executada por '%s' e não grava ",
               "artefato próprio: dentro da região ele só tem o valor do ponto da vez. ",
               "Peça o valor de '%s', que é onde o histórico da região vai pro store."),
        node, rid, rid), class = "tr_error_no_output")
    }
    rlang::abort(sprintf("Nó '%s' não está no plano.", node), class = "tr_error_unknown_node")
  }
  if (length(u$output_types) == 0) {
    rlang::abort(sprintf("Nó '%s' (%s) não tem porta de saída.", node, u$node_type),
                 class = "tr_error_no_output")
  }
  port <- port %||% names(u$output_types)[[1]]
  if (is.null(u$outputs[[port]])) {
    rlang::abort(sprintf("Porta de saída desconhecida em '%s': '%s'.", u$node_type, port),
                 class = "tr_error_unknown_port")
  }
  tr_store_get(store, u$outputs[[port]], tr_get_type(u$output_types[[port]], registry))
}
