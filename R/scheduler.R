#' Scheduler: a execução como máquina de passos.
#'
#' `tr_run_plan()` era um `while` com `Sys.sleep`, e `tr_server` o chamava
#' dentro de um `observeEvent` — o processo do Shiny ficava travado até o run
#' acabar, MESMO com o pool `mirai`. Três promessas do design dependiam de o
#' coordenador ficar livre e não existiam: cancelar unidade superada, mostrar
#' progresso enquanto roda, mostrar parcial. Aqui o estado é explícito e
#' `step()` faz um passo (despacha o que está pronto, coleta o que terminou).
#' Headless: `while (!s$step()) ...`. Shiny: `later::later(step)`.
#'
#' DOIS conjuntos, e a distinção importa:
#'   `done`    — terminou com valor bom (executou agora, ou veio do cache).
#'   `skipped` — não vai produzir valor (falhou, inválido, bloqueado).
#'
#' A versão anterior inferia "terminou" de "não está mais em `pending`" — e
#' uma unidade EM VOO também não está em `pending`. Com capacidade > 1 o
#' consumidor era despachado antes do produtor gravar, falhava com "chave
#' ausente", e — pior — esse erro virava handle no store: como a chave é
#' determinística, o erro falso ficava cacheado com a mesma autoridade de um
#' resultado bom, para sempre.
#' @export
tr_scheduler <- function(plan, registry = .tr_default_registry, store,
                         executor = tr_executor_sequential(),
                         on_event = function(ev) invisible(NULL),
                         run_id = NULL, inherit = list(), ctx_extra = NULL) {
  run_id <- run_id %||% .tr_entropy_hex(12L)
  st <- new.env(parent = emptyenv())
  st$done <- character(); st$skipped <- character(); st$results <- list()
  st$inflight <- list(); st$finished <- FALSE

  emit <- function(type, u, ...) {
    ev <- list(type = type, run_id = run_id, node = u$node,
               node_type = u$node_type, key = u$key, outputs = u$outputs)
    extra <- list(...)
    # SOBRESCREVE, em vez de concatenar: o parcial de um nó INTERIOR da região
    # sai com `node = <id do membro>`, e com `c()` o evento teria DUAS chaves
    # "node" — JSON de chave repetida, do qual o front lê a primeira (a do
    # colapso) e pinta o parcial do membro no card errado. `ev[nomes] <- extra`
    # preserva elemento NULL (o `fraction`/`message` de um progresso sem
    # fração), que é o que `ev[[nome]] <- NULL` apagaria.
    if (length(extra)) ev[names(extra)] <- extra
    on_event(ev)
  }

  # Os ids desta unidade que o RESTO DO GRAFO consome. Numa região são TODOS os
  # colapsos, e não só `u$node` (que é apenas o primeiro deles): o consumidor de
  # um segundo colapso esperava por um id que nunca entrava em `done`, ficava
  # sem ninguém em voo, e saía como "blocked_by = unreachable" pelo quebra-
  # -impasse do `dispatch()` — motivo não acionável, fora de `tr_plan_blocked()`,
  # e um artefato que o plano dizia estar em cache simplesmente não era lido.
  saidas_de <- function(u) u$region$collapse %||% u$node

  st$pending <- tr_plan_pending(plan)

  # Classificação inicial — idêntica à antiga, movida para o construtor.
  for (u in plan$units) {
    if (isTRUE(u$cached)) { emit("cached", u, handles = u$handles); st$done <- c(st$done, saidas_de(u)); next }
    if (isTRUE(u$failed)) {
      h <- u$handles[[1]]
      emit("failed", u, message = h$error$message, class = h$error$class, from_cache = TRUE)
      st$skipped <- c(st$skipped, saidas_de(u)); next
    }
    if (length(u$invalid) > 0) { emit("invalid", u, reason = paste(u$invalid, collapse = ", ")); st$skipped <- c(st$skipped, saidas_de(u)); next }
    if (length(u$blocked_by) > 0) { emit("blocked", u, blocked_by = I(as.character(u$blocked_by))); st$skipped <- c(st$skipped, saidas_de(u)); next }
  }

  # Adoção: unidades em voo de um run SUPERADO cuja chave continua no plano
  # novo. Sem isto, digitar `4`, `40`, `400` num param faria o run de `400`
  # redespachar o ramo que não mudou enquanto o daemon ainda o computa —
  # trabalho duplicado, e dois workers gravando a mesma chave.
  for (job in inherit) {
    i <- Position(function(p) identical(p$key, job$unit$key), st$pending)
    if (is.null(i) || is.na(i)) { executor$cancel(list(job$token)); next }
    u <- st$pending[[i]]; st$pending[[i]] <- NULL
    st$inflight[[u$node]] <- list(unit = u, token = job$token, t0 = job$t0, last_progress = NULL)
    emit("running", u, adopted = TRUE)
  }

  ready_of <- function(u) all(.tr_upstream_nodes(u) %in% st$done)

  prune <- function(bad_nodes) {
    frontier <- bad_nodes
    repeat {
      hit <- names(Filter(function(p) any(.tr_upstream_nodes(p) %in% frontier), st$pending))
      if (length(hit) == 0) break
      # A fronteira seguinte são as SAÍDAS do que acabou de cair, não os nomes
      # das unidades: quem consome o segundo colapso de uma região podada não
      # seria alcançado, e ficaria pendente pra sempre esperando por ele.
      seguinte <- character()
      for (nm in hit) {
        u <- st$pending[[nm]]
        emit("blocked", u, blocked_by = I(as.character(bad_nodes)))
        st$skipped <- c(st$skipped, saidas_de(u)); seguinte <- c(seguinte, saidas_de(u))
        st$pending[[nm]] <- NULL
      }
      frontier <- seguinte
    }
  }

  finish <- function() {
    st$finished <- TRUE
    on_event(list(type = "run_finished", run_id = run_id,
                  done = I(as.character(st$done)), skipped = I(as.character(st$skipped))))
    TRUE
  }

  dispatch <- function() {
    cap <- max(1L, executor$capacity())
    while (length(st$inflight) < cap && length(st$pending) > 0) {
      i <- Position(ready_of, st$pending)
      if (is.null(i) || is.na(i)) break
      u <- st$pending[[i]]; st$pending[[i]] <- NULL
      emit("running", u)
      # Progresso ÓRFÃO da mesma chave sai ANTES de submeter: a chave é
      # determinística, e um run anterior morto sem limpar (worker morto, sessão
      # fechada) deixa o arquivo no lugar. O primeiro poll deste run leria o
      # parcial daquele e o emitiria como se fosse deste — dado velho com a
      # autoridade de dado novo, que é o modo de falha que o store inteiro
      # evita. Aqui, e não na adoção: unidade adotada está EM VOO, e apagar o
      # progresso dela perderia o que o worker vivo acabou de publicar.
      .tr_clear_progress(store, u$key)
      # E o COMANDO órfão, pelo mesmo argumento e no mesmo lugar: a chave é
      # determinística, e um run morto sem limpar deixa o `control.json` de uma
      # região pausada no disco. O run novo nasceria pausado, com o card parado e
      # nada pra investigar. Aqui, e não na adoção: unidade adotada está EM VOO,
      # e pode estar legitimamente pausada neste instante.
      #
      # Consequência que vale dizer: um comando escrito ANTES do despacho é
      # perdido de propósito — o botão comanda o run em voo, não o próximo.
      .tr_control_clear(store, u$key)
      # `ctx_extra` desce até o `.ctx` do worker (`.tr_make_ctx()`), e é o único
      # caminho por onde os botões de cadência (parcial e checkpoint) chegam a um
      # run de verdade: quem chama `.tr_run_unit()` direto é só o teste.
      st$inflight[[u$node]] <- list(unit = u,
                                    token = executor$submit(u, registry, store, ctx_extra),
                                    t0 = Sys.time(), last_progress = NULL)
    }
    if (length(st$inflight) == 0 && length(st$pending) > 0) {
      for (nm in names(st$pending)) emit("blocked", st$pending[[nm]], blocked_by = I("unreachable"))
      st$skipped <- c(st$skipped, unlist(lapply(st$pending, saidas_de), use.names = FALSE))
      st$pending <- list()
    }
  }

  collect <- function() {
    progressed <- FALSE
    for (nm in names(st$inflight)) {
      job <- st$inflight[[nm]]; u <- job$unit
      res <- executor$collect(job$token)
      if (is.null(res)) {
        # Ainda rodando: progresso e parcial saem do store por polling. É o
        # custo aceito em worker.R por não abrir um segundo canal.
        p <- tr_progress(store, u$key)
        if (!is.null(p) && !identical(p, job$last_progress)) {
          st$inflight[[nm]]$last_progress <- p
          if (!is.null(p$partial)) emit("partial", u, handle = p$partial)
          # Um `partial` por NÓ que mudou. `ni`, e não `nm`: reusar o nome do
          # laço de fora trocaria a unidade em voo no `st$inflight` pelo id do
          # membro no resto da iteração. Só o que mudou, porque a região
          # republica o mapa INTEIRO a cada passo: sem o filtro, uma região de
          # dez membros emitiria dez eventos por poll, cada um redesenhando um
          # card que não mudou.
          #
          # O `node` do evento é um nó INTERIOR da região, que não tem unidade
          # nem chave própria — o front resolve pelo id do documento, que é a
          # chave com que ele já indexa o estado dos cards.
          for (ni in names(p$nodes)) {
            if (identical(p$nodes[[ni]], job$last_progress$nodes[[ni]])) next
            emit("partial", u, node = ni, handle = p$nodes[[ni]])
          }
          emit("progress", u, fraction = p$fraction, message = p$message)
        }
        next
      }
      progressed <- TRUE
      st$inflight[[nm]] <- NULL
      if (isTRUE(res$ok)) {
        st$done <- c(st$done, saidas_de(u)); st$results[[u$node]] <- res$handles
        emit("done", u, duration = as.numeric(Sys.time() - job$t0, units = "secs"), handles = res$handles)
      } else if (identical(res$error$class, "tr_error_stream_stopped")) {
        # PARAR não é falhar: o usuário mandou a região parar, o driver deixou o
        # checkpoint e a chave de saída continua VAZIA. Cair no ramo de falha
        # abaixo gravaria `tr_store_put_error` sob toda chave de saída da região
        # — um erro FALSO cacheado sob chave válida, que nenhum `tr_plan()`
        # recalcula: exatamente o modo de falha que a Fase 3 fechou, chegando
        # por outra estrada. `cancelled` é o evento que o `handoff()` já usa, e
        # ele não pinta card de vermelho.
        #
        # O jusante sai como BLOQUEADO (via `prune`), e não como falho: ele não
        # tem o que ler, mas ninguém errou. Sem o `prune` ele seria despachado,
        # morreria com "chave ausente" e o erro falso reapareceria um salto
        # adiante, agora sob a chave DELE.
        emit("cancelled", u, message = res$error$message)
        st$skipped <- c(st$skipped, saidas_de(u))
        prune(saidas_de(u))
      } else {
        for (k in unlist(u$outputs)) {
          tr_store_put_error(store, k, res$error$message, class = res$error$class,
                             traceback = res$error$traceback, node_type = u$node_type,
                             collections = u$collections)
        }
        emit("failed", u, message = res$error$message, class = res$error$class)
        st$skipped <- c(st$skipped, saidas_de(u))
        prune(saidas_de(u))
      }
      .tr_clear_progress(store, u$key)
    }
    progressed
  }

  # Um passo. Devolve TRUE quando o run terminou.
  step <- function() {
    if (st$finished) return(TRUE)
    dispatch()
    collect()
    # Coletar pode ter liberado dependentes: despachar de novo no MESMO passo
    # evita uma volta inteira do laço externo por nível do DAG.
    dispatch()
    if (length(st$pending) == 0 && length(st$inflight) == 0) return(finish())
    FALSE
  }

  # Entrega o run a um sucessor. Cancela o que não está em `keys`, devolve
  # os jobs sobreviventes para `inherit` do scheduler novo e encerra este
  # SEM `run_finished` — o run novo é quem fecha.
  handoff <- function(keys) {
    survivors <- list()
    for (nm in names(st$inflight)) {
      job <- st$inflight[[nm]]
      if (job$unit$key %in% keys) survivors[[length(survivors) + 1]] <- job
      else { executor$cancel(list(job$token)); emit("cancelled", job$unit); .tr_clear_progress(store, job$unit$key) }
    }
    for (nm in names(st$pending)) emit("cancelled", st$pending[[nm]])
    st$inflight <- list(); st$pending <- list(); st$finished <- TRUE
    survivors
  }

  structure(list(
    run_id = run_id, plan = plan,
    step = step, handoff = handoff,
    finished = function() st$finished,
    inflight = function() length(st$inflight),
    result = function() list(run_id = run_id, plan = plan, results = st$results,
                             done = st$done, skipped = st$skipped)
  ), class = "tr_scheduler")
}

.tr_clear_progress <- function(store, key) {
  unlink(file.path(store$root, "progress", paste0(key, ".json")))
}
