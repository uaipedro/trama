#' Documento em JSON.
#'
#' JSON canônico e permanente: o documento tem kilobytes, nunca vai ser o
#' gargalo, e ser texto legível é o que permite escrever e editar um grafo
#' direto com um LLM. O que é pesado (artefatos) nunca mora aqui.
#'
#' Listas nomeadas vazias precisam sair como `{}` e não `[]` — inclusive
#' `params` de cada nó, não só os mapas de topo. Um `params: []` volta do
#' parse como lista sem nome e `params[["k"]]` silenciosamente não acha nada.
#' @noRd
.tr_empty_obj <- function(x) if (length(x) == 0) stats::setNames(list(), character(0)) else x

#' Documento como JSON canônico — a forma que trafega pro front e vai pro disco.
#' @export
tr_doc_json <- function(doc) {
  doc <- .tr_as_doc(doc)
  out <- unclass(doc)
  out$nodes <- .tr_empty_obj(lapply(out$nodes, function(n) {
    n$params <- .tr_empty_obj(n$params); n
  }))
  out$collections <- .tr_empty_obj(out$collections)
  out$ui$positions <- .tr_empty_obj(out$ui$positions)
  out$ui$sizes <- .tr_empty_obj(out$ui$sizes)
  out$ui$views <- .tr_empty_obj(out$ui$views)
  out$ui$frames <- .tr_empty_obj(out$ui$frames)
  out$ui$folds <- .tr_empty_obj(out$ui$folds)
  out$ui$notes <- .tr_empty_obj(out$ui$notes)
  out$edges <- unname(out$edges)
  jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE)
}

#' Grava o documento em `path` como JSON.
#' @export
tr_doc_write <- function(doc, path) {
  writeLines(tr_doc_json(doc), path)
  invisible(doc)
}

#' `simplifyVector = FALSE` preserva a forma dos mapas aninhados (`nodes` como
#' objeto), mas transforma TODO array JSON em `list()` — inclusive o valor de
#' um param que era `c("x","y")`, que volta como `list("x","y")`.
#'
#' Isso não é cosmético: (a) a chave de conteúdo mudaria entre a sessão que
#' criou o grafo e a que o reabriu, dando cache miss universal — o oposto do
#' que o store promete; (b) o `fn` do nó receberia uma lista onde a assinatura
#' espera um vetor. Arrays de escalares homogêneos voltam a ser vetor aqui.
#' @noRd
.tr_resimplify <- function(x) {
  if (!is.list(x) || length(x) == 0 || !is.null(names(x))) return(x)
  ok <- all(vapply(x, function(e) is.atomic(e) && length(e) == 1L && !is.list(e), logical(1)))
  if (!ok) return(x)
  types <- unique(vapply(x, function(e) typeof(e), ""))
  if (length(types) > 1L) return(x)
  unlist(x, use.names = FALSE)
}

#' Lê um documento a partir de texto JSON, validando o formato e reconstituindo vetores achatados pelo parse.
#' @export
tr_doc_parse <- function(txt) {
  doc <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  if (!identical(as.integer(doc$format %||% NA_integer_), 1L)) {
    rlang::abort(sprintf("Formato de documento não suportado: %s.", doc$format %||% "ausente"),
                 class = "tr_error_bad_format")
  }
  doc$format <- 1L
  doc$rev <- as.integer(doc$rev %||% 0L)
  doc$nodes <- lapply(doc$nodes %||% list(), function(n) {
    n$params <- lapply(n$params %||% list(), .tr_resimplify)
    n$seed <- if (is.null(n$seed)) NULL else as.integer(n$seed)
    n
  })
  doc$edges <- doc$edges %||% list()
  doc$collections <- doc$collections %||% list()
  doc$ui <- doc$ui %||% list()
  # Os mapas nascem aqui mesmo quando o JSON não os traz: `lapply` de lista
  # vazia perde os nomes, e sem `.tr_empty_obj` o mapa reapareceria como `[]` na
  # gravação seguinte — a armadilha que o documento de tela sempre reintroduz.
  doc$ui$positions <- .tr_empty_obj(lapply(doc$ui$positions %||% list(), .tr_resimplify))
  doc$ui$sizes <- .tr_empty_obj(lapply(doc$ui$sizes %||% list(), .tr_resimplify))
  doc$ui$views <- .tr_empty_obj(lapply(doc$ui$views %||% list(), function(v) as.character(v)[[1]]))
  doc$ui$frames <- .tr_empty_obj(doc$ui$frames %||% list())
  doc$ui$folds <- .tr_empty_obj(doc$ui$folds %||% list())
  doc$ui$notes <- .tr_empty_obj(doc$ui$notes %||% list())
  structure(doc, class = "tr_doc")
}

#' Lê um documento de um arquivo `.json`.
#' @export
tr_doc_read <- function(path) tr_doc_parse(paste(readLines(path, warn = FALSE), collapse = "\n"))

#' Revalida um documento inteiro contra um registro.
#'
#' Existe por causa de documento escrito à mão ou por LLM — que é uma
#' modalidade de autoria que o formato JSON convida. Por isso checa as mesmas
#' invariantes que as ops garantem (ciclo, aridade de porta, input obrigatório
#' desconectado): um documento escrito à mão viola qualquer uma delas
#' trivialmente, e sem checar aqui quem descobre é o executor — o "erro longe
#' da causa" que a checagem na edição existe pra evitar.
#'
#' Tipo desconhecido NÃO é erro fatal: vira nó órfão, visível na tela, pra que
#' o usuário veja o que falta em vez de receber um documento que não abre.
#' @export
tr_doc_validate <- function(doc, registry = .tr_default_registry) {
  doc <- .tr_as_doc(doc)
  problems <- list()
  add <- function(kind, ...) problems[[length(problems) + 1]] <<- c(list(kind = kind), list(...))

  for (id in names(doc$nodes)) {
    n <- doc$nodes[[id]]
    if (is.null(registry$nodes[[n$type]])) { add("unknown_node_type", node = id, type = n$type); next }
    spec <- registry$nodes[[n$type]]
    if (!identical(as.integer(n$type_version %||% spec$version), spec$version)) {
      add("version_drift", node = id, type = n$type, from = n$type_version, to = spec$version)
    }
    for (p in setdiff(names(n$params), names(spec$params))) add("unknown_param", node = id, param = p)
    for (p in names(n$params)) {
      err <- tryCatch({ .tr_check_param_value(spec$params[[p]], n$params[[p]], p); NULL },
                      error = function(e) conditionMessage(e))
      if (!is.null(err)) add("bad_param_value", node = id, param = p, message = err)
    }
  }

  arity <- list()
  for (e in doc$edges) {
    from <- doc$nodes[[e$from$node]]; to <- doc$nodes[[e$to$node]]
    if (is.null(from) || is.null(to)) { add("dangling_edge", edge = .tr_edge_key(e)); next }
    fs <- registry$nodes[[from$type]]; ts <- registry$nodes[[to$type]]
    if (is.null(fs) || is.null(ts)) next
    op <- fs$outputs[[e$from$port]]; ip <- ts$inputs[[e$to$port]]
    if (is.null(op) || is.null(ip)) { add("unknown_port", edge = .tr_edge_key(e)); next }
    if (!tr_compatible(op$type, ip$type, registry)) {
      add("type_mismatch", edge = .tr_edge_key(e), from = op$type, to = ip$type)
    }
    slot <- paste0(e$to$node, ":", e$to$port)
    arity[[slot]] <- c(arity[[slot]] %||% 0L, 1L)
  }
  for (slot in names(arity)) {
    parts <- strsplit(slot, ":", fixed = TRUE)[[1]]
    n <- doc$nodes[[parts[[1]]]]; if (is.null(n)) next
    spec <- registry$nodes[[n$type]]; if (is.null(spec)) next
    ip <- spec$inputs[[parts[[2]]]]; if (is.null(ip)) next
    if (!isTRUE(ip$multiple) && sum(arity[[slot]]) > 1L) {
      add("port_overloaded", node = parts[[1]], port = parts[[2]], count = sum(arity[[slot]]))
    }
  }

  # Input obrigatório sem aresta: o executor falharia com "argumento ausente",
  # dentro da função do domínio, sem dizer qual porta faltou.
  connected <- vapply(doc$edges, function(e) paste0(e$to$node, ":", e$to$port), "")
  for (id in names(doc$nodes)) {
    spec <- registry$nodes[[doc$nodes[[id]]$type]]; if (is.null(spec)) next
    for (pn in names(spec$inputs)) {
      if (isTRUE(spec$inputs[[pn]]$required) && !paste0(id, ":", pn) %in% connected) {
        add("missing_required_input", node = id, port = pn)
      }
    }
  }

  cyc <- .tr_find_cycle(doc)
  if (!is.null(cyc)) add("cycle", edge = paste(cyc, collapse = " -> "))

  # As cinco recusas de região de fluxo (Decisão 7 do desenho: "erro na
  # validação de aresta, alto e cedo") — antes só apareciam como ABORT de
  # `tr_plan()`, e um `to_stream` sem colapso poisonava o documento inteiro em
  # silêncio (autosave grava, `tr_plan()` aborta, e nem o card errado nem os
  # outros cards do MESMO documento davam pista nenhuma). `.tr_stream_problems()`
  # é só um GRAFO WALK (`.tr_stream_detect()`, a metade não-abortante da
  # detecção) — sem fingerprint, sem disco — então cabe custar aqui, em toda
  # op. `tryCatch`: um tipo de nó desconhecido já virou `unknown_node_type`
  # acima, mas `.tr_stream_detect()` não sabe disso e chama `tr_get_node()`
  # (que ABORTA pra tipo desconhecido) por dentro — sem a guarda, um documento
  # com um nó órfão E uma região faria `tr_doc_validate()` em si abortar, que é
  # exatamente o modo de falha que esta função existe pra evitar em quem a
  # chama.
  problems <- c(problems, tryCatch(.tr_stream_problems(doc, registry), error = function(e) list()))

  problems
}
