#' Exporta um fluxo como código R independente do Trama.
#'
#' Diferente de [tr_flow_code()], que gera a DSL usada para reconstruir o
#' documento no editor, esta função gera chamadas diretas às funções R dos nós.
#' O script resultante não cria registro, documento, plano ou store do Trama;
#' ele pode ser aberto e executado como qualquer outro script R, desde que os
#' pacotes das coleções estejam instalados.
#'
#' Fluxos ponto a ponto ainda dependem do executor do Trama e, por isso, são
#' recusados em vez de gerar um script que calcularia outra coisa.
#'
#' @param doc Documento ou [tr_flow()] a exportar.
#' @param registry Registro que resolve os nós e adaptadores do documento.
#' @param format `"r"` para um script R ou `"quarto"` para um documento Quarto.
#' @param title Título usado no documento Quarto.
#' @return Uma string única pronta para ser gravada em um arquivo `.R` ou `.qmd`.
#' @export
tr_export_code <- function(doc, registry = .tr_default_registry,
                           format = c("r", "quarto"), title = "Análise") {
  format <- match.arg(format)
  doc <- .tr_as_doc(doc)
  .tr_export_check_supported(doc, registry)

  plan <- tr_plan(doc, targets = names(doc$nodes), registry = registry)
  order <- .tr_plan_node_order(plan)
  vars <- .tr_export_vars(doc, registry)
  temporaries <- character()
  lines <- c(
    "# Código R gerado pelo editor.",
    "# Execute a partir da pasta do projeto para preservar caminhos relativos.",
    ""
  )

  for (id in order) {
    node <- doc$nodes[[id]]
    spec <- tr_get_node(node$type, registry)
    args <- character()

    for (pn in names(spec$inputs)) {
      edges <- Filter(function(e) identical(e$to$node, id) && identical(e$to$port, pn), doc$edges)
      if (!length(edges)) next
      edges <- edges[order(vapply(edges, function(e) as.integer(e$index %||% 1L), integer(1)))]
      value <- vapply(edges, function(e) {
        expr <- vars[[e$from$node]][[e$from$port]]
        from <- tr_get_node(doc$nodes[[e$from$node]]$type, registry)
        if (!identical(from$outputs[[e$from$port]]$type, spec$inputs[[pn]]$type)) {
          adapter <- tr_adapter_for(from$outputs[[e$from$port]]$type, spec$inputs[[pn]]$type, registry)
          expr <- sprintf("%s(%s)", .tr_export_fn_ref(adapter$fn), expr)
        }
        expr
      }, "")
      rhs <- if (isTRUE(spec$inputs[[pn]]$multiple)) {
        sprintf("list(%s)", paste(value, collapse = ", "))
      } else value[[1]]
      args <- c(args, sprintf("%s = %s", .tr_export_name(pn), rhs))
    }

    for (pn in names(node$params)) {
      args <- c(args, sprintf("%s = %s", .tr_export_name(pn), .tr_export_value(node$params[[pn]])))
    }
    if (isTRUE(spec$stochastic) && ".seed" %in% names(formals(spec$fn))) {
      args <- c(args, sprintf(".seed = %s", .tr_export_value(node$seed)))
    }

    call <- sprintf("%s(%s)", .tr_export_fn_ref(spec$fn), paste(args, collapse = ", "))
    ports <- names(spec$outputs)
    if (!length(ports)) {
      lines <- c(lines, call, "")
    } else if (length(ports) == 1L) {
      lines <- c(lines, sprintf("%s <- %s", vars[[id]][[ports]], call), "")
    } else {
      used <- c(unlist(vars, use.names = FALSE), temporaries)
      result <- make.unique(c(used, make.names(paste0(id, "_resultado"))), sep = "_")[[length(used) + 1L]]
      temporaries <- c(temporaries, result)
      lines <- c(lines, sprintf("%s <- %s", result, call))
      lines <- c(lines, vapply(ports, function(pn) {
        sprintf("%s <- %s[[%s]]", vars[[id]][[pn]], result, .tr_export_value(pn))
      }, ""), "")
    }
  }

  script <- paste(lines, collapse = "\n")
  if (identical(format, "r")) return(script)
  paste0("---\n", "title: ", .tr_export_yaml(title), "\nformat: html\n---\n\n",
         "```{r}\n#| echo: true\n\n", script, "\n```\n")
}

.tr_export_check_supported <- function(doc, registry) {
  for (node in doc$nodes) {
    spec <- tr_get_node(node$type, registry)
    if (isTRUE(spec$online) || any(vapply(c(spec$inputs, spec$outputs), function(p) isTRUE(p$stream), logical(1)))) {
      rlang::abort(
        sprintf("O nó '%s' usa execução ponto a ponto e ainda não pode ser exportado como script R independente.", node$id),
        class = "tr_error_export_stream"
      )
    }
    context <- formals(spec$fn)[[".ctx"]]
    if (!is.null(context) && identical(context, quote(expr = ))) {
      rlang::abort(
        sprintf("O nó '%s' exige o contexto de execução do Trama e não pode ser exportado como script R independente.", node$id),
        class = "tr_error_export_context"
      )
    }
  }
  invisible(doc)
}

.tr_export_vars <- function(doc, registry) {
  used <- character()
  out <- list()
  for (id in names(doc$nodes)) {
    ports <- names(tr_get_node(doc$nodes[[id]]$type, registry)$outputs)
    values <- character(length(ports))
    names(values) <- ports
    for (pn in ports) {
      stem <- if (length(ports) == 1L) id else paste(id, pn, sep = "_")
      name <- make.names(stem)
      name <- make.unique(c(used, name), sep = "_")[[length(used) + 1L]]
      used <- c(used, name)
      values[[pn]] <- name
    }
    out[[id]] <- values
  }
  out
}

.tr_export_name <- function(name) {
  if (make.names(name) == name && !name %in% c("NA", "NULL", "TRUE", "FALSE", "Inf", "NaN")) name
  else paste0("`", gsub("`", "\\\\`", name, fixed = TRUE), "`")
}

.tr_export_value <- function(value) paste(capture.output(dput(value)), collapse = "\n")

.tr_export_yaml <- function(value) {
  paste0('"', gsub('"', '\\\\"', as.character(value), fixed = TRUE), '"')
}

#' Referência que um script independente pode chamar. Funções exportadas usam
#' `pacote::função`; helpers de coleção ainda usam `pacote:::função`, pois o
#' script continua independente do Trama mesmo quando a coleção escolheu não
#' publicar esse detalhe da sua API.
#' @noRd
.tr_export_fn_ref <- function(fn) {
  env <- environment(fn)
  pkg <- if (is.environment(env) && isNamespace(env)) getNamespaceName(env) else NULL
  if (!is.null(pkg)) {
    names <- ls(env, all.names = TRUE)
    found <- names[vapply(names, function(name) identical(get(name, envir = env), fn), logical(1))]
    if (length(found)) {
      exported <- found[found %in% getNamespaceExports(pkg)]
      if (length(exported)) return(paste0(pkg, "::", exported[[1]]))
      return(paste0(pkg, ":::", found[[1]]))
    }
  }
  if (is.primitive(fn)) {
    base_names <- getNamespaceExports("base")
    found <- base_names[vapply(base_names, function(name) identical(get(name, envir = baseenv()), fn), logical(1))]
    if (length(found)) return(paste0("base::", found[[1]]))
  }
  paste0("(", paste(deparse(fn), collapse = "\n"), ")")
}
