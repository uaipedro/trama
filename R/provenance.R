#' Proveniência dos resultados em cache de um fluxo.
#'
#' Para cada nó do fluxo, de onde veio o valor que o editor mostra: a chave,
#' quando foi produzido, com que versão de R e de cada pacote de coleção. A
#' chave de cache NÃO inclui versão de dependência (ver `hash.R`), então um
#' resultado pode ter sido calculado por outra versão da coleção — esta tabela
#' é como se descobre, e é o que um relatório reprodutível precisa citar.
#'
#' Nós sem resultado em cache saem com `status = "pendente"`; resultados
#' gravados antes de o handle guardar o ambiente saem com `r = NA`.
#'
#' @param project Projeto (`tr_project()`).
#' @param flow Nome do fluxo em `flows/`.
#' @return data.frame com `node`, `type`, `port`, `status`, `key`, `created`,
#'   `r` e uma coluna `pkg_<pacote>` por pacote registrado no handle.
#' @export
tr_provenance <- function(project, flow = "main") {
  doc <- tr_doc_migrate(tr_doc_read(file.path(project$flows_dir, paste0(flow, ".json"))),
                        project$registry)
  plan <- tr_plan(doc, targets = names(doc$nodes), registry = project$registry,
                  store = project$store, settings = project$settings)
  rows <- list()
  for (u in plan$units) {
    for (port in names(u$outputs)) {
      key <- u$outputs[[port]]
      h <- tr_store_handle(project$store, key)
      env <- h$env
      row <- list(
        node = u$node, type = u$node_type, port = port,
        status = if (is.null(h)) "pendente" else if (!is.null(h$error)) "erro" else "ok",
        key = key,
        created = if (is.null(h$created)) as.POSIXct(NA) else as.POSIXct(h$created, origin = "1970-01-01"),
        r = env$r %||% NA_character_
      )
      for (p in names(env$packages)) row[[paste0("pkg_", p)]] <- env$packages[[p]] %||% NA_character_
      rows[[length(rows) + 1L]] <- row
    }
  }
  if (!length(rows)) {
    return(data.frame(node = character(), type = character(), port = character(), status = character(),
                      key = character(), created = as.POSIXct(character()), r = character()))
  }
  cols <- unique(unlist(lapply(rows, names)))
  out <- lapply(cols, function(cl) {
    v <- lapply(rows, function(r) r[[cl]] %||% NA)
    if (cl == "created") do.call(c, v) else unlist(v)
  })
  out <- stats::setNames(out, cols)
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}
