#' Superfície R para escrever um fluxo — açúcar sobre as ops, nada mais.
#'
#' O documento sempre pôde ser construído no console aplicando listas de ops,
#' que é ótimo para máquina e ruim para gente. O manifesto mira "referência de
#' como se constrói fluxo lógico em R": isso exige que o fluxo se ESCREVA em R
#' idiomático, com pipe, e que o mesmo texto produza o mesmo JSON que a
#' interface. Esta camada não tem semântica própria — cada verbo vira uma op e
#' passa pela mesma validação — para que não existam dois caminhos que possam
#' divergir.
#'
#' Referência a porta: `"no:porta"`. Os dois pontos são proibidos em id de nó
#' (`.tr_check_node_id`), então a separação é inequívoca; `"."` não serviria,
#' porque id pode conter ponto.
#' @export
tr_flow <- function(registry = .tr_default_registry, doc = tr_doc()) {
  structure(list(doc = doc, registry = registry), class = "tr_flow")
}

.tr_as_doc <- function(x) if (inherits(x, "tr_flow")) x$doc else x

.tr_flow_apply <- function(flow, op) {
  flow$doc <- tr_doc_apply(flow$doc, op, flow$registry); flow
}

#' Acrescenta um nó. `...` são params (nomeados). `from` liga a saída de um ou
#' mais nós existentes às portas de entrada obrigatórias ainda livres deste,
#' na ordem declarada — o caso comum de "encadeia" sem precisar nomear porta.
#' @export
tr_add <- function(flow, id, type, ..., from = NULL, label = NULL, seed = NULL, position = NULL) {
  params <- list(...)
  if (length(params) && (is.null(names(params)) || any(!nzchar(names(params))))) {
    rlang::abort("Params de tr_add() precisam ser nomeados.", class = "tr_error_bad_op")
  }
  .tr_check_param_shadow(type, flow$registry, names(params),
                         c(from = !is.null(from), label = !is.null(label),
                           seed = !is.null(seed), position = !is.null(position)))
  op <- list(op = "add_node", id = id, type = type, params = params)
  if (!is.null(label)) op$label <- label
  if (!is.null(seed)) op$seed <- seed
  if (!is.null(position)) op$position <- position
  flow <- .tr_flow_apply(flow, op)
  for (src in from) flow <- .tr_flow_autolink(flow, src, id)
  flow
}

#' Param de nó cujo nome colide com um argumento formal de `tr_add()`.
#'
#' `tr_add()` recebe params por `...`, então um param chamado `from`, `label`,
#' `seed` ou `position` nunca chega ao `...`: o R o casa com o formal de mesmo
#' nome, e o valor vira metadado do nó — ou, no caso de `from`, uma ARESTA.
#'
#' `tr_add("r", "data/rename", from = "regiao")` liga o nó `regiao` a `r` e
#' deixa o param `from` por preencher. Se `regiao` existir no fluxo, isso não
#' falha: faz outra coisa, calada. É o modo de falha que este pacote trata como
#' o pior de todos, e é por isso que a checagem existe.
#'
#' Dispara só quando o formal foi REALMENTE passado — um nó pode declarar um
#' param `from` e continuar sendo usado normalmente por `tr_add()` para os
#' outros params. `type` não entra: o R já falha alto ali, porque o tipo do nó
#' fica sem posição.
#'
#' Descoberto escrevendo a ajuda de `data/rename` (param `from`) e
#' `data/convert` (param `type`).
#' @noRd
.tr_check_param_shadow <- function(type, registry, passados, formais) {
  spec <- tryCatch(tr_get_node(type, registry), error = function(e) NULL)
  if (is.null(spec)) return(invisible(NULL))
  ambiguos <- intersect(names(spec$params), names(formais)[formais])
  ambiguos <- setdiff(ambiguos, passados)
  if (length(ambiguos)) {
    rlang::abort(
      sprintf(paste0("Em '%s', %s é param do nó E argumento de tr_add(): o valor ",
                     "foi para o argumento da DSL, não para o param. Use tr_set() ",
                     "para preencher o param."),
              type, paste(sprintf("'%s'", ambiguos), collapse = " e ")),
      class = "tr_error_param_shadow")
  }
  invisible(NULL)
}

.tr_split_ref <- function(ref) {
  parts <- strsplit(ref, ":", fixed = TRUE)[[1]]
  list(node = parts[[1]], port = if (length(parts) > 1L) parts[[2]] else NULL)
}

#' Liga `from` a `to`. Porta omitida = primeira saída / primeira entrada.
#' @export
tr_link <- function(flow, from, to, index = NULL) {
  f <- .tr_split_ref(from); t <- .tr_split_ref(to)
  reg <- flow$registry; doc <- flow$doc
  fspec <- tr_get_node(.tr_node_or_abort(doc, f$node)$type, reg)
  tspec <- tr_get_node(.tr_node_or_abort(doc, t$node)$type, reg)
  f$port <- f$port %||% names(fspec$outputs)[[1]]
  t$port <- t$port %||% names(tspec$inputs)[[1]]
  op <- list(op = "connect", from_node = f$node, from_port = f$port, to_node = t$node, to_port = t$port)
  if (!is.null(index)) op$index <- index
  .tr_flow_apply(flow, op)
}

#' `from` sem porta de destino: a PRIMEIRA entrada obrigatória livre e
#' compatível recebe a ligação. Erro alto se não houver — ligar em silêncio
#' na porta "errada" é pior que não ligar.
#' @noRd
.tr_flow_autolink <- function(flow, src, id) {
  f <- .tr_split_ref(src); reg <- flow$registry; doc <- flow$doc
  fspec <- tr_get_node(.tr_node_or_abort(doc, f$node)$type, reg)
  f$port <- f$port %||% names(fspec$outputs)[[1]]
  out_type <- fspec$outputs[[f$port]]$type
  tspec <- tr_get_node(doc$nodes[[id]]$type, reg)
  taken <- vapply(Filter(function(e) e$to$node == id, doc$edges), function(e) e$to$port, "")
  for (pn in names(tspec$inputs)) {
    p <- tspec$inputs[[pn]]
    if (pn %in% taken && !isTRUE(p$multiple)) next
    if (tr_compatible(out_type, p$type, reg)) return(tr_link(flow, paste0(f$node, ":", f$port), paste0(id, ":", pn)))
  }
  rlang::abort(sprintf("Nenhuma entrada livre de '%s' aceita %s (%s).", id, src, out_type),
               class = "tr_error_type_mismatch")
}

#' Aplica `set_param` para cada `...` nomeado, um por um.
#' @export
tr_set <- function(flow, id, ...) {
  vals <- list(...)
  for (nm in names(vals)) flow <- .tr_flow_apply(flow, list(op = "set_param", node = id, name = nm, value = vals[[nm]]))
  flow
}

#' O `tr_doc` por trás de um `tr_flow` — a saída pronta pra `tr_run()`, `tr_doc_write()` etc.
#' @export
tr_flow_doc <- function(flow) flow$doc

#' @export
print.tr_flow <- function(x, ...) { cat("<tr_flow>\n"); print(x$doc); invisible(x) }

#' Gera o código R (DSL) que reconstrói o documento. É a outra metade da
#' promessa "diagrama e código são a mesma coisa": o que se monta na interface
#' se lê como R, e o que se escreve em R aparece na interface.
#'
#' Ordem topológica; params só quando diferem do default; `from =` quando a
#' ligação é a canônica (primeira saída -> primeira entrada obrigatória livre),
#' senão `tr_link()` explícito. Posições NÃO entram: são apresentação.
#'
#' Param cujo NOME é formal de `tr_add()` (`from`, `label`, `seed`, `position`,
#' e também `type`, `id`, `flow`) não pode sair dentro do `tr_add()`: o R casa
#' o nome com o formal, não com o param — é a armadilha que
#' `.tr_check_param_shadow()` denuncia do lado de quem escreve. Emitido
#' ingenuamente, `data/rename` (param `from`) saía como
#' `tr_add("ren", "data/rename", from = "mpg", to = "consumo", from = "ler")`:
#' argumento duplicado, código que não roda. Esses params saem num `tr_set()`
#' encadeado — o caminho que o próprio guard manda usar — e, quando o param
#' sombreado é `from`, as arestas do nó saem por `tr_link()` explícito, porque
#' o formal `from` continua vedado para aquele `tr_add()`.
#' @export
tr_flow_code <- function(doc, registry = .tr_default_registry, registry_expr = "reg") {
  doc <- .tr_as_doc(doc)
  plan <- tr_plan(doc, targets = names(doc$nodes), registry = registry)
  order <- names(plan$units)
  dep <- function(x) if (is.numeric(x) || is.logical(x)) paste(deparse(x), collapse = "") else deparse(x)
  # Tirado dos formais de `tr_add()`, e não de uma lista à parte, pra não haver
  # duas verdades: um formal novo passa a ser reservado aqui no mesmo commit.
  reservados <- setdiff(names(formals(tr_add)), "...")
  lines <- sprintf("tr_flow(%s)", registry_expr); links <- character(); sets <- character()
  for (id in order) {
    n <- doc$nodes[[id]]; spec <- tr_get_node(n$type, registry)
    sombra <- intersect(names(spec$params), reservados)
    args <- c(sprintf('"%s"', id), sprintf('"%s"', n$type))
    for (pn in names(n$params)) {
      if (identical(n$params[[pn]], spec$params[[pn]]$default)) next
      arg <- sprintf("%s = %s", pn, dep(n$params[[pn]]))
      if (pn %in% sombra) sets <- c(sets, sprintf('  tr_set("%s", %s)', id, arg))
      else args <- c(args, arg)
    }
    incoming <- Filter(function(e) e$to$node == id, doc$edges)
    incoming <- incoming[order(vapply(incoming, function(e) as.integer(e$index %||% 1L), 1L))]
    froms <- character()
    for (e in incoming) {
      canonical <- !("from" %in% sombra) && .tr_is_canonical_link(doc, registry, e, froms)
      if (canonical) froms <- c(froms, .tr_ref(e$from, tr_get_node(doc$nodes[[e$from$node]]$type, registry)))
      else links <- c(links, sprintf('  tr_link("%s:%s", "%s:%s"%s)', e$from$node, e$from$port, e$to$node, e$to$port,
                                     if (isTRUE(spec$inputs[[e$to$port]]$multiple)) sprintf(", index = %d", as.integer(e$index %||% 1L)) else ""))
    }
    if (length(froms)) args <- c(args, sprintf("from = %s", if (length(froms) == 1L) sprintf('"%s"', froms) else sprintf("c(%s)", paste(sprintf('"%s"', froms), collapse = ", "))))
    if (!identical(n$label, spec$label)) args <- c(args, sprintf('label = "%s"', n$label))
    if (isTRUE(spec$stochastic)) args <- c(args, sprintf("seed = %dL", n$seed))
    lines <- c(lines, sprintf("  tr_add(%s)", paste(args, collapse = ", ")))
  }
  paste(c(lines, links, sets), collapse = " |>\n")
}

.tr_ref <- function(from, spec) {
  if (identical(from$port, names(spec$outputs)[[1]])) from$node else paste0(from$node, ":", from$port)
}

#' A ligação é canônica se `.tr_flow_autolink` a reproduziria: simula a
#' escolha de porta que o autolink faria dado o que já foi ligado por `from`.
#' @noRd
.tr_is_canonical_link <- function(doc, registry, e, already) {
  tspec <- tr_get_node(doc$nodes[[e$to$node]]$type, registry)
  fspec <- tr_get_node(doc$nodes[[e$from$node]]$type, registry)
  out_type <- fspec$outputs[[e$from$port]]$type
  taken <- character()
  for (a in already) { r <- .tr_split_ref(a); taken <- c(taken, .tr_autolink_target(doc, registry, r$node, r$port %||% names(tr_get_node(doc$nodes[[r$node]]$type, registry)$outputs)[[1]], e$to$node, taken)) }
  identical(.tr_autolink_target(doc, registry, e$from$node, e$from$port, e$to$node, taken), e$to$port)
}

.tr_autolink_target <- function(doc, registry, fnode, fport, tnode, taken) {
  fspec <- tr_get_node(doc$nodes[[fnode]]$type, registry); tspec <- tr_get_node(doc$nodes[[tnode]]$type, registry)
  out_type <- fspec$outputs[[fport]]$type
  for (pn in names(tspec$inputs)) {
    p <- tspec$inputs[[pn]]
    if (pn %in% taken && !isTRUE(p$multiple)) next
    if (tr_compatible(out_type, p$type, registry)) return(pn)
  }
  NA_character_
}
