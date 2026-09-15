#' Executores: a mesma interface, duas estratégias.
#'
#' O executor é plugável de propósito. O sequencial é o default em teste e
#' headless — rápido, determinístico, sem processo extra e sem ramificação de
#' código entre "com UI" e "de script". O pool existe pra que o processo
#' interativo nunca bloqueie.
#'
#' Contrato:
#'   $submit(unit, registry, store) -> token
#'   $collect(token)                -> NULL enquanto não terminou, senão
#'                                     list(ok=, handles=|error=)
#'   $capacity()                    -> quantas unidades em voo ao mesmo tempo
#'   $cancel(tokens)                -> descarta trabalho em andamento
#'   $shutdown()

#' Executor sequencial: roda no próprio processo, na hora.
#'
#' `cancel()` é honesto sobre não fazer nada: cancelamento cooperativo em R
#' arbitrário não existe, e sem processo separado não há o que matar. O
#' contrato existe nos dois executores mesmo assim, pra que o protocolo não
#' precise mudar depois — mudar protocolo com o front pronto custa dobrado.
#' @export
tr_executor_sequential <- function() {
  structure(list(
    kind = "sequential",
    capacity = function() 1L,
    submit = function(unit, registry, store) {
      list(unit = unit, result = .tr_capture_unit(unit, registry, store))
    },
    collect = function(token) token$result,
    cancel = function(tokens) invisible(FALSE),
    shutdown = function() invisible(TRUE)
  ), class = "tr_executor")
}

#' Executor com pool de daemons (`mirai`).
#'
#' Cancelar = MATAR o daemon. Cancelamento cooperativo em R (e no C++ de um
#' `terra` da vida) não existe; matar é o único contrato honesto. Isso só é
#' aceitável porque o worker é sem estado: o que ele produziu de útil já está
#' no store sob a chave certa, e o que não terminou não deixa rastro (escrita
#' atômica, store.R). Projete o nó para o worker morrer a qualquer momento.
#' @export
tr_executor_pool <- function(n = 2L, registry = .tr_default_registry, setup = NULL, ...) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    rlang::abort(
      "tr_executor_pool() precisa do pacote 'mirai'. Instale-o, ou use tr_executor_sequential().",
      class = "tr_error_missing_mirai"
    )
  }
  # Coleção definida em `globalenv()` (console, testes — o "nível 1" da API,
  # preservado de propósito) NÃO é reconstruível num daemon: não há pacote a
  # carregar. Falhar aqui, alto e cedo, é melhor que um `miraiError` opaco por
  # unidade despachada, muito depois e sem relação com a causa.
  orphan <- Filter(function(c) is.null(c$package), registry$collections)
  if (length(orphan) > 0) {
    rlang::abort(sprintf(
      "Coleção(ões) %s não vieram de um pacote e não podem ser despachadas para daemons. Use tr_executor_sequential().",
      paste(vapply(orphan, function(c) c$id, ""), collapse = ", ")),
      class = "tr_error_collection_not_dispatchable")
  }
  mirai::daemons(n, ...)
  # `setup` roda UMA vez em cada daemon. Existe pelo fluxo de desenvolvimento:
  # com `pkgload::load_all()` não há pacote instalado, e `trama:::` dentro do
  # daemon não resolve nada. Em produção fica NULL — `asNamespace("trama")`
  # basta. Sem isto, o pool só era testável depois de `R CMD INSTALL`, ou
  # seja, nunca no ciclo rápido — e foi por isso que ficou "validado só por
  # leitura" até aqui.
  if (!is.null(setup)) mirai::everywhere(.expr = setup)
  structure(list(
    kind = "pool",
    capacity = function() n,
    submit = function(unit, registry, store) {
      # Só dado atravessa: a unidade é serializável (não carrega `fn` nem
      # ambiente — ver plan.R) e o worker resolve o nó no registro dele.
      # Só dado atravessa: a unidade é serializável (não carrega `fn` nem
      # ambiente — ver plan.R) e o worker reconstrói o registro a partir dos
      # PACOTES de origem. O id da coleção não serve pra isso: id e nome de
      # pacote só coincidem por acidente (`trama.terrain` traz `terrain`).
      mirai::mirai(
        { trama:::.tr_capture_unit(unit, trama::tr_registry_for(pkgs), store) },
        unit = unit, store = store,
        pkgs = unname(vapply(registry$collections, function(c) c$package, ""))
      )
    },
    collect = function(token) {
      if (mirai::unresolved(token)) return(NULL)
      val <- token$data
      if (mirai::is_error_value(val)) {
        # Interrupção (cancel) e morte do daemon chegam ambas como erro do
        # mirai; classificar permite ao front não pintar de vermelho um nó que
        # o próprio coordenador mandou parar.
        cls <- if (inherits(val, "miraiInterrupt")) "tr_error_cancelled" else "tr_error_worker_died"
        return(list(ok = FALSE, error = list(message = conditionMessage(val), class = cls)))
      }
      val
    },
    cancel = function(tokens) {
      for (t in tokens) try(mirai::stop_mirai(t), silent = TRUE)
      invisible(TRUE)
    },
    shutdown = function() { mirai::daemons(0); invisible(TRUE) }
  ), class = "tr_executor")
}

#' Reconstrói um registro a partir dos NOMES DE PACOTE — como o worker
#' recupera o catálogo sem receber funções pela fronteira.
#' @export
tr_registry_for <- function(packages) {
  reg <- tr_registry()
  for (p in packages) tr_use(p, registry = reg)
  reg
}

#' Roda uma unidade capturando a falha como VALOR.
#'
#' Erro nunca sobe como exceção daqui: vira um handle de erro no store, e o
#' plano seguinte marca o jusante como bloqueado em vez de reenfileirá-lo. Sem
#' isso, uma ponta quebrada faz o mundo reexecutar a cada edição e o erro
#' reaparece N vezes em vez de uma.
#'
#' A condição atravessa a fronteira de processo com classe e traceback
#' preservados — desenhado aqui, não descoberto depois.
#' @noRd
.tr_capture_unit <- function(unit, registry, store) {
  # `trace_back()` DENTRO do handler do tryCatch roda depois do desempilhamento
  # — os frames do `fn` já não existem, e o que se captura é o caminho interno
  # do próprio trama. `withCallingHandlers` roda ANTES de desempilhar, que é o
  # único jeito de o traceback apontar pro código que de fato quebrou.
  # `bottom = 0` é necessário; sem ele o trace volta vazio.
  tr <- NULL
  tryCatch(
    withCallingHandlers(
      list(ok = TRUE, handles = .tr_run_unit(unit, registry, store)),
      error = function(e) tr <<- rlang::trace_back(bottom = 0)
    ),
    error = function(e) {
      list(ok = FALSE, error = list(
        message = conditionMessage(e),
        class = class(e)[1],
        traceback = if (is.null(tr)) NULL
                    else paste(utils::capture.output(print(tr)), collapse = "\n")
      ))
    }
  )
}

#' @export
print.tr_executor <- function(x, ...) {
  cat(sprintf("<tr_executor> %s, capacidade %d\n", x$kind, x$capacity()))
  invisible(x)
}
