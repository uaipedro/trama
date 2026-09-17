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
#'
#' `ctx_extra` é a PORTA DOS AJUSTES DO RUN: uma lista de escalares que desce
#' inteira até o `.ctx` do worker (`.tr_make_ctx()`), pelo scheduler e pelo
#' executor, e vale igual nos dois executores. Hoje é lida por:
#'   `publish_every`    — segundos entre publicações de parcial (default 0.1)
#'   `checkpoint_every` — passos entre checkpoints da região (default 100; `0`
#'                        desliga a serialização periódica do acumulador)
#'
#' Não é `settings`, e a distinção é de desenho: `settings` é conteúdo do projeto
#' e ENTRA no hash (o tema muda o gráfico), enquanto `ctx_extra` é ajuste de
#' sessão e não toca documento nem chave (Decisão 9) — se tocasse, baixar a
#' cadência de checkpoint recomputaria o fluxo inteiro que ela existe pra
#' proteger. Também não é `tr_stream_command()`: lá vão comandos VIVOS, que
#' mudam durante o run; aqui vão ajustes fixados quando o run começa.
#'
#' Existe como argumento explícito, e não como opção global, porque duas sessões
#' sobre o mesmo store são o caso normal (é como os testes encenam o segundo
#' processo) e um `options()` faria a cadência de uma vazar na outra.
#' @export
tr_run <- function(doc, targets = NULL, registry = .tr_default_registry, store,
                   executor = tr_executor_sequential(),
                   on_event = function(ev) invisible(NULL), run_id = NULL, settings = NULL,
                   ctx_extra = NULL) {
  doc <- .tr_as_doc(doc)
  plan <- tr_plan(doc, targets, registry, store, settings = settings)
  .tr_warn_ctx_extra(ctx_extra)
  tr_run_plan(plan, registry, store, executor, on_event, run_id, ctx_extra)
}

#' Executa um plano já montado — separado de `tr_run()` porque o coordenador
#' interativo (E5) monta o plano a cada edição pra decidir o que cancelar, e
#' só então manda executar.
#' @export
tr_run_plan <- function(plan, registry = .tr_default_registry, store,
                        executor = tr_executor_sequential(),
                        on_event = function(ev) invisible(NULL), run_id = NULL,
                        ctx_extra = NULL) {
  s <- tr_scheduler(plan, registry, store, executor, on_event, run_id,
                    ctx_extra = ctx_extra)
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
#'
#' `ctx_extra` está aqui porque pedir o valor de um nó no console RODA o grafo, e
#' uma região de dez mil pontos é justamente o que alguém pede assim: sem o
#' argumento, o único jeito de baixar a cadência de checkpoint num run de console
#' seria não usar `tr_value()`.
#' @export
tr_value <- function(doc, node, registry = .tr_default_registry, store,
                     port = NULL, executor = tr_executor_sequential(), settings = NULL,
                     ctx_extra = NULL) {
  res <- tr_run(doc, targets = node, registry = registry, store = store, executor = executor,
                settings = settings, ctx_extra = .tr_warn_ctx_extra(ctx_extra))
  u <- res$plan$units[[node]]
  # Membro INTERIOR de região não tem unidade própria: a região é uma unidade
  # só, nomeada pelo PRIMEIRO colapso (`tr_plan()`, `region$collapse[[1]]`).
  # Sem este ramo `u` era NULL, `length(u$output_types)` dava 0, e o `sprintf`
  # com `u$node_type = NULL` devolvia `character(0)` — o abort saía com a
  # mensagem VAZIA, e quem pedisse o valor de um nó de dentro da região não
  # recebia pista nenhuma. É pra isto que `plan$region_of` existe; era o único
  # lugar do pacote que devia lê-lo.
  if (is.null(u)) {
    rid <- res$plan$region_of[[node]]
    if (!is.null(rid)) {
      # MAS `region_of[[node]]` não distinguia um membro INTERIOR de um
      # SEGUNDO COLAPSO da mesma região: os dois batem em `u == NULL`, porque
      # só o primeiro colapso vira entrada de `plan$units`. Uma região com
      # dois colapsos GRAVA artefato para os dois (`tr_plan()`: "cada saída de
      # cada colapso derivada da MESMA chave de unidade") — só que o segundo
      # mora em `ru$outputs`, sob o nome que `region$outputs` (Fase 3) dá a
      # ele, não em `plan$units[[node]]`. Sem checar isso primeiro, todo
      # segundo colapso caía na mensagem de "nó interior", que é FALSA para
      # ele (ele grava, sim) e redireciona pro primeiro colapso — cujo
      # artefato pode ter colunas diferentes (é OUTRO valor).
      ru <- res$plan$units[[rid]]
      om <- ru$region$outputs[[node]]
      if (!is.null(om)) {
        p <- port %||% names(om)[[1]]
        idx <- match(p, names(om))
        if (is.na(idx)) {
          rlang::abort(sprintf("Porta de saída desconhecida em '%s': '%s'.", node, p),
                       class = "tr_error_unknown_port")
        }
        nm <- om[[idx]]
        return(tr_store_get(store, ru$outputs[[nm]], tr_get_type(ru$output_types[[nm]], registry)))
      }
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

#' Comando: limpa SÓ o(s) handle(s) de erro de um nó, preservando o checkpoint.
#'
#' O gesto explícito que faltou depois da Fase 5. Uma falha grava handle de
#' ERRO sob toda porta de saída da unidade (`R/scheduler.R`, o ramo de falha de
#' `collect()`, via `tr_store_put_error`); `tr_plan()` marca a unidade
#' `failed`, e o scheduler a serve direto de `skipped`/`from_cache` —
#' `tr_run()` sozinho, no MESMO store, nunca redespacha, mesmo com um
#' `ckpt.rds` bom esperando no disco (`R/stream-driver.R`: falha no `step`,
#' falha no colapso e worker morto guardam o checkpoint DE PROPÓSITO). O único
#' jeito público de tirar o handle de erro era `tr_bust()` — que apaga
#' `<store>/stream/` inteiro (`R/hash.R`), porque bustar significa "o código
#' mudou sem a chave mudar", e aí retomar serviria um histórico metade velho,
#' metade novo. Usado contra um erro comum, `tr_bust()` jogaria fora os pontos
#' bons do checkpoint pela mesma chamada que limpa o erro do último ponto —
#' exatamente o custo que a retenção do checkpoint existe pra evitar.
#'
#' `tr_retry()` faz só a metade que falta: tira o(s) handle(s) de erro da
#' unidade e NUNCA toca `<store>/stream/<chave da unidade>/`. Não é
#' `tr_bust()` com um filtro a mais — é o oposto dele: um preserva o
#' checkpoint de propósito, o outro apaga de propósito, e os dois precisam
#' continuar existindo porque servem casos diferentes ("o ponto 10.000 errou,
#' quero retomar dali" vs. "mudei o código, o checkpoint velho não presta
#' mais").
#'
#' Recusa apagar um handle BOM (`tr_handle_failed()` falso): um artefato
#' válido não é candidato a retry — apagá-lo seria um `tr_bust()` disfarçado,
#' silencioso, sem o comentário que explica a perda do checkpoint. A recusa é
#' TRANSACIONAL: se qualquer porta de saída da unidade guarda um artefato bom,
#' a chamada aborta ANTES de apagar qualquer handle de erro das outras portas
#' — uma região de dois colapsos não pode sair com um retry parcial.
#'
#' Nó sem handle nenhum (nunca rodou, ou já foi limpo) é NO-OP silencioso: a
#' mesma forma que `tr_stream_command()` escolheu pra chave sem diretório
#' (`R/stream-control.R`) — clique perdido não é erro.
#'
#' Membro INTERIOR de uma região não tem unidade nem chave de saída própria
#' (ver o comentário de `tr_value()` acima, e `plan$region_of`): pedir retry
#' dele significa "retry a região", resolvida pelo mesmo mapa que `tr_value()`
#' já usa pra leitura — o efeito observável é o de pedir retry por qualquer um
#' dos colapsos dela.
#'
#' `settings` existe pelo mesmo motivo que em `tr_value()`: sem ele o plano
#' calcularia a chave com o tema embutido, e talvez não achasse a MESMA
#' unidade (nem as MESMAS chaves de saída) que o run de verdade usou.
#'
#' Não roda nada — só planeja, pra achar a unidade e as chaves, e mexe no
#' store. É COMANDO, não op de documento: não toca `doc`, `rev` nem o log de
#' undo (Decisão 10), do mesmo jeito que pause/step/stop de
#' `R/stream-control.R`.
#' @return `invisible(TRUE)` se limpou pelo menos um handle de erro;
#'   `invisible(FALSE)` se não havia nada a limpar (nó sem handle).
#' Vale em nó COMUM também, e não só em região: ali limpa o erro cacheado e o
#' nó roda de novo — só não há de onde retomar, porque nó comum não tem
#' checkpoint. É por isso que a recusa não fala de checkpoint: o `tr_bust()` que
#' ela indica serve para os dois casos, e o que ele apaga a mais (o diretório
#' `stream/`) só existe num deles.
#'
#' SEM entrada no barramento do Shiny, de propósito: `transport.R` só decodifica
#' comando VIVO cuja chave o front já tem em mão (pause, step, stop). Retentar
#' precisa de `doc` + `registry` + `settings` para resolver um id de nó numa
#' unidade, que é conhecimento de plano dentro do barramento — o que aquele
#' arquivo recusa por desenho. Consequência a registrar em vez de descobrir: um
#' botão de retentar no front precisa de mensagem nova ou de um caminho por
#' chave, e isso é trabalho da fase da interface.
#'
#' @export
tr_retry <- function(doc, node, registry = .tr_default_registry, store, settings = NULL) {
  doc <- .tr_as_doc(doc)
  plan <- tr_plan(doc, targets = node, registry = registry, store = store, settings = settings)
  u <- plan$units[[node]]
  if (is.null(u)) {
    rid <- plan$region_of[[node]]
    if (!is.null(rid)) u <- plan$units[[rid]]
  }
  if (is.null(u)) {
    rlang::abort(sprintf("Nó '%s' não está no plano.", node), class = "tr_error_unknown_node")
  }

  # Primeiro passo: só CONFERE. Nada é apagado aqui — é o que torna a recusa
  # abaixo transacional (ver o roxygen).
  keys <- unlist(u$outputs)
  a_limpar <- character()
  for (k in keys) {
    h <- tr_store_handle(store, k)
    if (is.null(h)) next
    if (!tr_handle_failed(h)) {
      rlang::abort(sprintf(
        paste0("Nó '%s' tem artefato BOM sob a chave '%s': tr_retry() só limpa handle de ",
               "ERRO, nunca um resultado válido. Se a intenção é forçar recomputação porque o ",
               "código mudou sem a chave mudar, o comando certo é tr_bust()."),
        node, k), class = "tr_error_retry_good_artifact")
    }
    a_limpar <- c(a_limpar, k)
  }

  # Segundo passo: agora que nenhuma porta é boa, apaga os handles de erro.
  # `<store>/stream/<chave>/` nunca é tocado aqui — é o ponto do comando.
  for (k in a_limpar) .tr_drop_key(store, k)
  invisible(length(a_limpar) > 0)
}
