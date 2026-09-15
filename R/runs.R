#' Log de execução: uma linha JSON por evento, um arquivo por run.
#'
#' O motor já emite duração real e chave por unidade; até aqui isso morria no
#' front. Gravar é o que dá base ao "Monitorar" do manifesto sem inventar
#' superfície nova — e é telemetria honesta para medir o que o cache poupa.
#' Handles ficam de fora: são pesados e já estão no store.
#' @export
tr_run_logger <- function(root, run_id) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(root, paste0(run_id, ".jsonl"))
  con <- file(path, open = "a", encoding = "UTF-8")
  list(
    log = function(ev) {
      ev$handles <- NULL; ev$handle <- NULL; ev$outputs <- NULL
      ev$at <- as.numeric(Sys.time())
      writeLines(jsonlite::toJSON(ev, auto_unbox = TRUE, null = "null", digits = NA), con)
      flush(con)
    },
    close = function() try(close(con), silent = TRUE),
    path = path
  )
}

#' Lê todos os runs de um projeto como data.frame (um evento por linha).
#' @export
tr_runs <- function(project) {
  files <- list.files(file.path(project$root, ".trama", "runs"), pattern = "\\.jsonl$", full.names = TRUE)
  rows <- lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    lapply(lines, function(l) tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL))
  })
  rows <- Filter(Negate(is.null), unlist(rows, recursive = FALSE))
  if (!length(rows)) return(data.frame(run_id = character(), node = character(), node_type = character(),
                                       type = character(), duration = numeric(), at = numeric()))
  pick <- function(r, f, d) r[[f]] %||% d
  data.frame(
    run_id = vapply(rows, pick, "", f = "run_id", d = NA_character_),
    node = vapply(rows, pick, "", f = "node", d = NA_character_),
    node_type = vapply(rows, pick, "", f = "node_type", d = NA_character_),
    type = vapply(rows, function(r) r$unit_type %||% r$type %||% NA_character_, ""),
    duration = vapply(rows, function(r) as.numeric(r$duration %||% NA_real_), 0),
    at = vapply(rows, function(r) as.numeric(r$at %||% NA_real_), 0),
    stringsAsFactors = FALSE)
}

#' GC do projeto: preserva tudo que QUALQUER flow do projeto alcança. `tr_store_gc`
#' recebia `keep` de um documento só; com dois flows no mesmo projeto, coletar
#' pelo primeiro apagaria o cache do segundo. Falha de leitura de um flow não
#' impede o GC dos outros, mas aborta a coleta — melhor não coletar do que
#' apagar o que um flow ilegível alcançava. Os `settings` do projeto entram no
#' plano pelo mesmo motivo: sem eles a chave de quem usa tema do projeto seria
#' outra, e o cache vivo pareceria órfão.
#' @export
tr_project_gc <- function(project, max_age_days = 7) {
  keep <- character()
  for (f in list.files(project$flows_dir, pattern = "\\.json$", full.names = TRUE)) {
    doc <- tryCatch(tr_doc_read(f), error = function(e) NULL)
    if (is.null(doc)) return(invisible(0L))
    plan <- tryCatch(tr_plan(doc, targets = names(doc$nodes), registry = project$registry,
                                     settings = project$settings),
                     error = function(e) NULL)
    if (is.null(plan)) return(invisible(0L))
    keep <- c(keep, tr_plan_keys(plan))
  }
  tr_store_gc(project$store, keep = unique(keep), max_age_days = max_age_days)
}
