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

  # As regiões de fluxo saem UMA vez, antes do DFS. `.tr_stream_regions()`
  # varre o grafo inteiro e recusa região malformada; chamá-la por nó pagaria a
  # varredura a cada visita e repetiria a recusa. Documento sem porta de fluxo
  # devolve `list()` sem percorrer nada, então o caminho de sempre não paga por
  # isto — e não fica sabendo que fluxo existe.
  regions <- .tr_stream_regions(doc, registry)
  region_of <- list(); region_unit <- list()
  for (r in regions) for (id in r$nodes) {
    region_of[[id]] <- r$id
    # A unidade é nomeada pelo PRIMEIRO colapso (`collapse` já sai ordenado):
    # o nome aparece em `plan$units`, no evento do scheduler e no `tr_value()`,
    # então tem que ser determinístico e estável sob renome de outro nó.
    region_unit[[id]] <- r$collapse[[1]]
  }

  out_keys <- list(); units <- list(); visiting <- character(); bad <- character()
  done_regions <- character()

  # Resolve as arestas que chegam numa porta: sobe pelo montante, escolhe o
  # adaptador pelo par de tipos e devolve as referências que o worker vai ler.
  # Extraído pra que a região atravesse EXATAMENTE este caminho — duas cópias
  # divergiriam, e a que ficasse atrás calcularia chave de entrada diferente da
  # do resto do plano.
  resolve_group <- function(es, port) {
    # Porta variádica: TODAS as arestas, na ordem de `index` — é pra isso que
    # `index` existe. Sem ordenar, a ordem seria a de inserção na lista de
    # arestas e o resultado dependeria da ordem em que o usuário editou.
    es <- es[order(vapply(es, function(e) as.integer(e$index %||% 1L), integer(1)))]
    if (!isTRUE(port$multiple)) es <- es[1]
    refs <- lapply(es, function(e) {
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
    # A chave da SAÍDA já codifica a porta de origem: sem isso, um nó com
    # duas saídas daria a mesma chave upstream para as duas, e trocar
    # `split.treino` por `split.teste` não mudaria nada a jusante — servir
    # resultado errado em silêncio.
    list(refs = refs,
         keys = vapply(refs, function(r) r$key, ""),
         prints = lapply(refs, function(r) .tr_adapter_print(registry, r$adapter)))
  }

  resolve_region <- function(region) {
    if (region$id %in% done_regions) return(invisible(NULL))
    # `visiting` com TODOS os membros: um ciclo que entrasse na região por uma
    # aresta externa e voltasse por outra sai como `tr_error_cycle`, e não como
    # recursão infinita.
    visiting <<- c(visiting, region$nodes)
    specs <- lapply(stats::setNames(region$nodes, region$nodes),
                    function(id) tr_get_node(doc$nodes[[id]]$type, registry))

    inputs <- list(); ext_keys <- list(); ext_prints <- list()
    upstream <- character(); invalid <- character(); members <- list()

    # `region$external` (da detecção) é a ÚNICA verdade sobre qual aresta entra
    # de fora. Re-derivar aqui com `!(e$from$node %in% region$nodes)` criava um
    # SEGUNDO predicado de "externa", que pode divergir do da detecção — é
    # exatamente a classe de bug do Crítico da Fase 2, quando o `external`
    # perdeu uma aresta e este lado, que a derivava sozinho, não viu diferença.
    # `external` preserva a ordem de `doc$edges`, então agrupar por `nó:porta` e
    # ordenar por `index` continua sendo trabalho daqui.
    de_fora_keys <- vapply(region$external, .tr_edge_key, "")
    eh_de_fora <- function(e) .tr_edge_key(e) %in% de_fora_keys

    for (id in region$nodes) {
      spec <- specs[[id]]; node <- doc$nodes[[id]]
      incoming <- by_target[[id]] %||% list()
      sources <- list()
      for (pn in names(spec$inputs)) {
        port <- spec$inputs[[pn]]
        es <- Filter(function(e) e$to$port == pn, incoming)
        if (length(es) == 0) {
          # Mesma régua do nó solto, e pelo mesmo motivo — só que o nome leva o
          # nó: "x solto" numa região de dez nós não diz onde mexer.
          if (isTRUE(port$required)) {
            invalid <- c(invalid, paste0("missing_required_input:", id, ":", pn))
          }
          next
        }
        es <- es[order(vapply(es, function(e) as.integer(e$index %||% 1L), integer(1)))]
        if (!isTRUE(port$multiple)) es <- es[1]
        nm <- paste0(id, ":", pn)
        de_fora <- Filter(eh_de_fora, es)
        if (length(de_fora) > 0) {
          g <- resolve_group(de_fora, port)
          upstream <- c(upstream, vapply(g$refs, function(r) r$node, ""))
          # Entrada externa da região: referência no formato de sempre, só que
          # nomeada `nó:porta` — uma região tem portas de nós diferentes, e um
          # mapa por porta faria `a:x` e `b:x` colidirem. O separador é `:`, e
          # não `.`, porque `:` é proibido em id de nó (`.tr_check_node_id`):
          # com ponto, o nó `a.b` na porta `c` e o nó `a` na porta `b.c` dariam
          # o mesmo nome, e um sobrescreveria o outro — input errado, calado.
          inputs[[nm]] <- if (isTRUE(port$multiple)) g$refs else g$refs[[1]]
          ext_keys[[nm]] <- g$keys
          ext_prints[[nm]] <- g$prints
        }
        pos <- 0L
        sources[[pn]] <- lapply(es, function(e) {
          if (eh_de_fora(e)) {
            # A referência de fora mora em `inputs`, não aqui: repeti-la criaria
            # duas verdades sobre de qual chave o valor vem. `pos` é a posição
            # dela entre as externas da MESMA porta, que é o que permite
            # remontar a ordem do `index` numa porta variádica meio interna,
            # meio externa.
            pos <<- pos + 1L
            list(from = "external", input = nm, pos = pos)
          } else {
            out_type <- specs[[e$from$node]]$outputs[[e$from$port]]$type
            ad <- if (identical(out_type, port$type)) NULL
                  else tr_adapter_for(out_type, port$type, registry)
            list(from = "internal", node = e$from$node, port = e$from$port, type = out_type,
                 adapter = if (is.null(ad)) NULL else list(from = ad$from, to = ad$to))
          }
        })
      }
      members[[id]] <- list(
        node_type = node$type, node_version = spec$version,
        params = .tr_effective_params(spec, node, settings),
        seed = node$seed, online = isTRUE(spec$online), stochastic = isTRUE(spec$stochastic),
        role = if (id %in% region$source) "source"
               else if (id %in% region$collapse) "collapse" else "lifted",
        inputs = sources)
    }

    # O tipo da saída do colapso é o único que produz artefato sob esta chave:
    # `preview`, `store` e `summary` dele são código gravado no store. O tipo de
    # porta interna fica fora — nada dela vai ao disco.
    type_prints <- lapply(stats::setNames(region$collapse, region$collapse), function(cid)
      lapply(specs[[cid]]$outputs, function(p) .tr_type_fingerprint(registry, p$type)))

    # Membro IMPURO e membro VOLÁTIL: a liftabilidade isenta nó `online` por
    # completo (Fase 2), então os dois chegam até aqui sem recusa. Tratados como
    # `tr_plan()` trata um nó solto, e pelos mesmos dois motivos: sem o
    # `fingerprint()` do impuro a região serve dado velho quando o mundo externo
    # muda; sem o `nonce` do volátil ela cacheia um histórico que tinha que ser
    # recomputado sempre.
    externals <- list()
    for (id in region$nodes) {
      spec <- specs[[id]]
      if (isTRUE(spec$pure)) next
      fp <- spec$fingerprint
      externals[[id]] <- if (length(formals(fp)) >= 2L) {
        fp(members[[id]]$params, .tr_fp_ctx(store))
      } else {
        fp(members[[id]]$params)
      }
    }
    nonce <- if (any(vapply(specs, function(s) isTRUE(s$volatile), logical(1)))) {
      .tr_entropy_hex(16L)
    }

    unit_key <- .tr_region_key(region, members, specs, registry, ext_keys, ext_prints,
                               type_prints, externals, nonce)

    # Mais de um colapso na mesma região é legítimo (dois resumos do mesmo
    # fluxo) e é UMA execução: uma unidade, e cada saída de cada colapso
    # derivada da MESMA chave de unidade. Emitir uma unidade por colapso faria
    # a região rodar duas vezes.
    multi <- length(region$collapse) > 1L
    outs <- list(); out_types <- character(); omap <- list()
    for (cid in region$collapse) {
      ports <- names(specs[[cid]]$outputs)
      # Colapso sem porta de saída (um nó que só exibe o histórico) grava sob a
      # porta vazia, como qualquer nó terminal em `tr_plan()`.
      if (length(ports) == 0) ports <- ""
      nms <- vapply(ports, function(p) .tr_region_out_name(cid, p, multi), "", USE.NAMES = FALSE)
      # As chaves saem de `.tr_out_key()`, não de reler `outs[[n]]`: no colapso
      # sem porta de saída o nome é `""`, e `outs[[""]]` devolve NULL em R mesmo
      # tendo sido gravado por `outs[[""]] <- k`. `out_keys[[cid]]` virava
      # `list(NULL)` — inofensivo só porque esse colapso não tem consumidor.
      ks <- vapply(nms, function(n) .tr_out_key(unit_key, n), "", USE.NAMES = FALSE)
      for (i in seq_along(ports)) {
        outs[[nms[[i]]]] <- ks[[i]]
        if (nzchar(ports[[i]])) out_types[[nms[[i]]]] <- specs[[cid]]$outputs[[ports[[i]]]]$type
      }
      omap[[cid]] <- stats::setNames(nms, ports)
      out_keys[[cid]] <<- stats::setNames(as.list(ks), ports)
    }

    handles <- if (is.null(store)) list() else lapply(outs, function(k) tr_store_handle(store, k))
    failed <- any(vapply(handles, tr_handle_failed, logical(1)))
    cached <- length(handles) > 0 && !failed &&
      all(vapply(handles, function(h) !is.null(h), logical(1)))
    blocked_by <- if (cached) character() else intersect(upstream, bad)

    units[[region$collapse[[1]]]] <<- list(
      kind = "stream_region", node = region$collapse[[1]],
      node_type = "trama/stream_region",
      # As coleções de TODOS os membros, e não só a do colapso. `tr_bust()` é a
      # válvula documentada do gap "atualizar dependência externa não muda a
      # chave", e ela filtra por coleção lendo `node_type` do handle: com
      # "trama/stream_region" a região lia como coleção "trama", e
      # `tr_bust(store, "minha_colecao")` não a alcançava — recomputava todo nó
      # comum e servia o histórico de dez mil pontos do código de antes do
      # upgrade, calado, pra sempre. É um CONJUNTO porque uma região mistura
      # coleções de verdade (colapso `data/*` com membro `models/*`), e bustar
      # `models` tem que alcançá-la.
      collections = sort(unique(vapply(region$nodes,
                                       function(id) .tr_collection_of(doc$nodes[[id]]$type), ""))),
      key = unit_key, outputs = outs, output_types = out_types,
      inputs = inputs, params = list(), seed = NULL,
      # `.ctx` é contrato da UNIDADE, não de `fn` membro nenhum: nó elevado tem
      # `.ctx` proibido (Fase 2) e `init`/`step` não o recebem.
      #
      # Quem honra o pedido é `.tr_run_region()`, e não o bloco de
      # `.tr_run_unit()` que lê este campo nos nós comuns: o despacho pra o
      # driver é a primeira coisa lá (e continua sendo, pelo motivo escrito em
      # `worker.R`), então é o driver que constrói o ctx. Ele publica a fração do
      # passo e o parcial de cada membro sob a chave DESTA unidade, que é onde o
      # `collect()` do scheduler faz o poll.
      wants_ctx = TRUE,
      # Não existe "a seed da região": cada membro leva a sua em `region$nodes`.
      wants_seed = FALSE,
      cached = cached, failed = failed,
      handles = if (cached || failed) handles else NULL,
      blocked_by = blocked_by, invalid = invalid,
      # Cada nó da região, em ordem topológica, com o que o worker precisa pra
      # reconstruir a execução do próprio registro. Nada de `fn` aqui, pela
      # mesma razão de sempre (ver o cabeçalho deste arquivo).
      region = list(id = region$id, source = region$source, order = region$nodes,
                    nodes = members, collapse = region$collapse, outputs = omap))

    # TODOS os colapsos entram em `bad`: são os únicos nós da região que o resto
    # do grafo consome, e é pelo id deles que o jusante calcula `blocked_by`.
    if (failed || length(blocked_by) > 0 || length(invalid) > 0) {
      bad <<- c(bad, region$collapse)
    }
    done_regions <<- c(done_regions, region$id)
    visiting <<- setdiff(visiting, region$nodes)
    invisible(NULL)
  }

  resolve <- function(id, via_edge = TRUE) {
    if (!is.null(out_keys[[id]])) return(out_keys[[id]])
    if (id %in% visiting) rlang::abort(sprintf("Ciclo no plano: %s.", id), class = "tr_error_cycle")
    node <- doc$nodes[[id]]
    if (is.null(node)) rlang::abort(sprintf("Nó '%s' não existe.", id), class = "tr_error_unknown_node")

    rid <- region_of[[id]]
    if (!is.null(rid)) {
      region <- regions[[rid]]
      # Colapso: a região é UMA unidade, e as chaves das saídas dele saem dela.
      if (id %in% region$collapse) { resolve_region(region); return(out_keys[[id]]) }
      # Membro INTERIOR pedido como ALVO é legítimo, não erro: `tr_doc_terminals()`
      # entrega todo nó sem aresta de saída, e um acumulador que não alimenta
      # ninguém é um deles — abortar aqui tornaria improgramável um documento que
      # a validação da região aceita. Computá-lo É executar a região; o artefato
      # sai no colapso, e nó interior não tem chave própria porque não grava nada.
      if (!via_edge) { resolve_region(region); return(list()) }
      # Chegar aqui por ARESTA é impossível hoje: a aresta que sai de nó interior
      # para fora da região é exatamente o que `tr_error_stream_escapes` recusa na
      # validação. Rede de segurança pra um furo futuro lá — sem ela, o consumidor
      # de fora receberia uma chave que ninguém grava.
      rlang::abort(sprintf(
        paste0("'%s' é nó interior da região de fluxo '%s' e não grava artefato: ",
               "uma aresta o consome de fora da região."), id, rid),
        class = "tr_error_stream_escapes")
    }

    visiting <<- c(visiting, id)
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
      g <- resolve_group(es, port)
      upstream <- c(upstream, vapply(g$refs, function(r) r$node, ""))
      inputs[[pn]] <- if (isTRUE(port$multiple)) g$refs else g$refs[[1]]
      input_keys[[pn]] <- g$keys
      adapter_prints[[pn]] <- g$prints
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
      # `kind` existe pro scheduler e pro worker despacharem por ele em vez de
      # por `is.null(u$region)`: um campo ausente é fácil de esquecer de testar,
      # e quem esquecer executa uma região como se fosse um nó.
      kind = "node",
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

  # `via_edge = FALSE` só aqui: alvo é pedido do usuário, aresta é dependência
  # de dados, e o membro interior de uma região se comporta diferente nos dois.
  for (t in targets) resolve(t, via_edge = FALSE)

  # `units` já sai em pós-ordem do DFS, que é topológica por construção
  # (um nó só é inserido depois de todos os seus inputs).
  structure(list(doc_rev = doc$rev, targets = targets, units = units,
                 keys = lapply(units, function(u) u$key),
                 # De cada membro de região para a unidade que o executa. Sem
                 # este mapa, quem procura um nó em `plan$units` pelo id (o
                 # console, a interface) não acha nó interior nenhum e não tem
                 # como saber por quê.
                 region_of = unlist(region_unit)), class = "tr_plan")
}

#' Nome da saída de um colapso dentro da unidade-região.
#'
#' Com UM colapso o nome é a porta, e a unidade-região fica indistinguível de um
#' nó comum pra quem lê `outputs`/`output_types` — que é a promessa da fase. Com
#' mais de um, qualifica pelo nó: dois colapsos com uma porta `out` cada
#' colidiriam num mapa por porta, e o segundo artefato ficaria fora do plano e
#' fora do `keep` do GC, que o apagaria embaixo de quem o consome. O separador é
#' `:` pela mesma razão do nome da entrada externa: `.` é legal em id de nó, e
#' dois artefatos diferentes acabariam sob a mesma chave.
#' @noRd
.tr_region_out_name <- function(collapse, port, multi) {
  if (!multi) return(port)
  if (!nzchar(port)) collapse else paste0(collapse, ":", port)
}

#' Ordem topológica dos NÓS do plano, com os membros de cada região no lugar da
#' unidade que os executa.
#'
#' `plan$units` é a lista de unidades de EXECUÇÃO, e desde que a região virou
#' uma unidade ela não é mais a lista dos nós do documento. Quem quer nó — o
#' gerador de código, por exemplo — precisa desta.
#' @noRd
.tr_plan_node_order <- function(plan) {
  unlist(lapply(plan$units, function(u) {
    if (identical(u$kind, "stream_region")) u$region$order else u$node
  }), use.names = FALSE)
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
    st <- if (isTRUE(u$failed)) "erro" else if (length(u$invalid)) "inválido"
          else if (length(u$blocked_by)) "bloqueado" else if (isTRUE(u$cached)) "cache" else "rodar"
    cat(sprintf("  %-10s %-18s %-22s %s\n", st, u$node, u$node_type, substr(u$key, 1, 12)))
  }
  invisible(x)
}
