#' Plano de execução: fecho transitivo dos alvos, em ordem topológica, com a
#' chave de conteúdo de cada unidade já resolvida.
#'
#' Pull-based, herdado do insumo: nó fora do caminho do alvo não executa. O que
#' muda é que aqui o plano é um VALOR inspecionável — dá pra perguntar "o que
#' vai rodar e o que vem do cache" sem executar nada, comparar dois planos pra
#' ver o que uma edição afetou, e decidir cache hit no coordenador.
#'
#' **A unidade não carrega o `tr_node` vivo.** Carregar carregaria junto o
#' `fn` e o ambiente dele: numa coleção que mora em `globalenv()` (console,
#' testes — o "nível 1 da API" que E1 preserva de propósito), despachar isso
#' pra um daemon arrastaria o ambiente inteiro por unidade. O worker recebe
#' `node_type` e resolve no próprio registro. Efeito colateral bom: o plano
#' vira serializável, coerente com "o plano é um valor".
#'
#' `settings` (de `project$settings`) resolve os params de tema ANTES do hash.
#' Quem tem projeto em mãos TEM que passá-lo: plano sem ele calcula as chaves
#' com os temas embutidos, e o GC apagaria o cache vivo de quem usa tema do
#' projeto. `NULL` é a sessão sem projeto.
#' @export
tr_plan <- function(doc, targets = NULL, registry = .tr_default_registry, store = NULL,
                    settings = NULL) {
  doc <- .tr_as_doc(doc)
  targets <- targets %||% tr_doc_terminals(doc)
  by_target <- list()
  for (t in doc$edges) by_target[[t$to$node]] <- c(by_target[[t$to$node]], list(t))

  out_keys <- list(); units <- list(); visiting <- character(); bad <- character()

  resolve <- function(id) {
    if (!is.null(out_keys[[id]])) return(out_keys[[id]])
    if (id %in% visiting) rlang::abort(sprintf("Ciclo no plano: %s.", id), class = "tr_error_cycle")
    visiting <<- c(visiting, id)
    node <- doc$nodes[[id]]
    if (is.null(node)) rlang::abort(.tr_msg("plan.node_not_found", id), class = "tr_error_unknown_node")
    spec <- tr_get_node(node$type, registry)

    incoming <- by_target[[id]] %||% list()
    inputs <- list(); input_keys <- list(); adapter_prints <- list()
    upstream <- character(); invalid <- character()

    for (pn in names(spec$inputs)) {
      port <- spec$inputs[[pn]]
      es <- Filter(function(e) e$to$port == pn, incoming)
      if (length(es) == 0) {
        # Porta obrigatória solta: a unidade NÃO pode entrar na fila. Sem isto,
        # o worker falharia com "argumento ausente, sem padrão" dentro da
        # função do domínio — o "erro longe da causa" que a checagem na edição
        # existe pra evitar. Porta opcional simplesmente não é passada, e o
        # `fn` cai no próprio default.
        if (isTRUE(port$required)) invalid <- c(invalid, paste0("missing_required_input:", pn))
        next
      }
      # Porta variádica: TODAS as arestas, na ordem de `index` — é pra isso que
      # `index` existe. Sem ordenar, a ordem seria a de inserção na lista de
      # arestas e o resultado dependeria da ordem em que o usuário editou.
      es <- es[order(vapply(es, function(e) as.integer(e$index %||% 1L), integer(1)))]
      if (!isTRUE(port$multiple)) es <- es[1]

      resolved <- lapply(es, function(e) {
        up_outs <- resolve(e$from$node)
        up_spec <- tr_get_node(doc$nodes[[e$from$node]]$type, registry)
        out_type <- up_spec$outputs[[e$from$port]]$type
        # Adaptador entra por AQUI, na aresta — nunca como nó. Só o id viaja;
        # o worker resolve a função no registro dele.
        ad <- if (identical(out_type, port$type)) NULL
              else tr_adapter_for(out_type, port$type, registry)
        list(node = e$from$node, port = e$from$port,
             key = up_outs[[e$from$port]], type = out_type,
             adapter = if (is.null(ad)) NULL else list(from = ad$from, to = ad$to))
      })
      upstream <- c(upstream, vapply(resolved, function(r) r$node, ""))
      inputs[[pn]] <- if (isTRUE(port$multiple)) resolved else resolved[[1]]
      # A chave da SAÍDA já codifica a porta de origem: sem isso, um nó com
      # duas saídas daria a mesma chave upstream para as duas, e trocar
      # `split.treino` por `split.teste` não mudaria nada a jusante — servir
      # resultado errado em silêncio.
      input_keys[[pn]] <- vapply(resolved, function(r) r$key, "")
      adapter_prints[[pn]] <- lapply(resolved, function(r) {
        if (is.null(r$adapter)) NULL
        else .tr_print_cached(registry, paste0("ad:", r$adapter$from, "->", r$adapter$to), 1L,
                              tr_adapter_for(r$adapter$from, r$adapter$to, registry)$fn)
      })
    }

    # `fingerprint(params, ctx)` recebe o mesmo `path()` que o `fn` vai receber
    # em `.ctx`: se o nó resolve o caminho contra a raiz do projeto, a
    # impressão digital tem que olhar o MESMO arquivo — senão o mtime lido é o
    # de um arquivo que não existe, e a chave nunca muda.
    external <- if (isTRUE(spec$pure)) NULL else {
      fp <- spec$fingerprint
      if (length(formals(fp)) >= 2L) fp(.tr_effective_params(spec, node, settings), .tr_fp_ctx(store))
      else fp(.tr_effective_params(spec, node, settings))
    }
    nonce <- if (isTRUE(spec$volatile)) .tr_entropy_hex(16L) else NULL
    ordered <- if (length(input_keys)) input_keys[order(names(input_keys), method = "radix")] else input_keys
    type_prints <- lapply(spec$outputs, function(p) .tr_type_fingerprint(registry, p$type))
    fn_print <- .tr_print_cached(registry, spec$id, spec$version, spec$fn)

    unit_key <- .tr_unit_key(spec, node, ordered, adapter_prints, type_prints,
                             external, fn_print, nonce, settings)
    outs <- if (length(spec$outputs) == 0) {
      stats::setNames(list(.tr_out_key(unit_key, "")), "")
    } else {
      lapply(stats::setNames(names(spec$outputs), names(spec$outputs)),
             function(pn) .tr_out_key(unit_key, pn))
    }

    handles <- if (is.null(store)) list() else lapply(outs, function(k) tr_store_handle(store, k))
    failed <- any(vapply(handles, tr_handle_failed, logical(1)))
    cached <- length(handles) > 0 && !failed &&
      all(vapply(handles, function(h) !is.null(h), logical(1)))
    # Cache válido VENCE bloqueio: a chave é determinística, então se o
    # artefato existe sob ela o valor é o certo, independente do que aconteceu
    # a montante desde então.
    blocked_by <- if (cached) character() else intersect(upstream, bad)

    units[[id]] <<- list(
      node = id, node_type = node$type, node_version = spec$version,
      key = unit_key, outputs = outs,
      output_types = vapply(spec$outputs, function(p) p$type, ""),
      inputs = inputs, params = .tr_effective_params(spec, node, settings), seed = node$seed,
      wants_ctx = ".ctx" %in% names(formals(spec$fn)),
      wants_seed = ".seed" %in% names(formals(spec$fn)),
      cached = cached, failed = failed,
      # Handles ficam na unidade: ao reabrir um documento TUDO é cache, e sem
      # isso o front receberia zero preview e zero summary. Já foram lidos
      # aqui pra decidir o cache hit — descartá-los obrigaria a lê-los de novo.
      handles = if (cached || failed) handles else NULL,
      blocked_by = blocked_by, invalid = invalid
    )
    if (failed || length(blocked_by) > 0 || length(invalid) > 0) bad <<- c(bad, id)
    out_keys[[id]] <<- outs
    visiting <<- setdiff(visiting, id)
    outs
  }

  for (t in targets) resolve(t)

  # `units` já sai em pós-ordem do DFS, que é topológica por construção
  # (um nó só é inserido depois de todos os seus inputs).
  structure(list(doc_rev = doc$rev, targets = targets, units = units,
                 keys = lapply(units, function(u) u$key)), class = "tr_plan")
}

.tr_fp_ctx <- function(store) list(
  root = if (is.null(store)) NULL else store$project_root,
  path = function(p) if (is.null(store)) p else tr_store_path(store, p))

#' Unidades que não podem rodar: falharam, dependem de quem falhou, ou têm
#' porta obrigatória solta.
#' @export
tr_plan_blocked <- function(plan) {
  Filter(function(u) isTRUE(u$failed) || length(u$blocked_by) > 0 || length(u$invalid) > 0,
         plan$units)
}

#' O que de fato precisa rodar.
#' @export
tr_plan_pending <- function(plan) {
  Filter(function(u) !isTRUE(u$cached) && !isTRUE(u$failed) &&
           length(u$blocked_by) == 0 && length(u$invalid) == 0, plan$units)
}

#' Todas as chaves de artefato alcançáveis — o conjunto `keep` do `tr_store_gc()`.
#' @export
tr_plan_keys <- function(plan) unname(unlist(lapply(plan$units, function(u) unlist(u$outputs))))

#' @export
print.tr_plan <- function(x, ...) {
  cat(sprintf("<tr_plan> rev %s | %d unidade(s): %d em cache, %d a rodar, %d bloqueada(s)\n",
              x$doc_rev, length(x$units), sum(vapply(x$units, function(u) isTRUE(u$cached), TRUE)),
              length(tr_plan_pending(x)), length(tr_plan_blocked(x))))
  for (u in x$units) {
    # Só "inválido" sai do código: o catálogo existe para tirar ACENTO de
    # string, e os outros quatro estados já são ASCII.
    st <- if (isTRUE(u$failed)) "erro" else if (length(u$invalid)) .tr_msg("plan.status_invalido")
          else if (length(u$blocked_by)) "bloqueado" else if (isTRUE(u$cached)) "cache" else "rodar"
    cat(sprintf("  %-10s %-18s %-22s %s\n", st, u$node, u$node_type, substr(u$key, 1, 12)))
  }
  invisible(x)
}
