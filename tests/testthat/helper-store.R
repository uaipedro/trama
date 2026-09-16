# Coleção de teste com persistência declarada no TIPO — ainda sem domínio
# real: `t/box` é só uma lista com um número dentro, guardada em RDS.
store_collection <- function() {
  boxed <- tr_type("t/box", version = 1L,
    store   = function(x, path) saveRDS(x, path),
    restore = function(path) readRDS(path),
    ext     = "rds",
    preview = function(x, ctx) tr_preview("t/box", data = list(v = x$v)),
    summary = function(x) list(v = x$v))
  tr_collection(
    id = "t", version = "1.0.0", types = list(boxed),
    nodes = list(
      tr_node("t/const", fn = function(v) list(v = v),
              description = "Devolve uma caixa com o número escolhido.",
              outputs = list(out = "t/box"), params = list(v = tr_param_num(1))),
      tr_node("t/inc", fn = function(x, by) list(v = x$v + by),
              description = "Soma uma constante ao valor da caixa que recebe.",
              inputs = list(x = "t/box"), outputs = list(out = "t/box"),
              params = list(by = tr_param_num(1))),
      tr_node("t/sum", fn = function(xs) list(v = sum(vapply(xs, function(e) e$v, 0))),
              description = "Soma os valores de todas as caixas ligadas.",
              inputs = list(xs = tr_port("t/box", multiple = TRUE)),
              outputs = list(out = "t/box")),
      tr_node("t/boom", fn = function(x) stop("explodiu"),
              description = "Falha de propósito ao receber uma caixa.",
              inputs = list(x = "t/box"), outputs = list(out = "t/box")),
      tr_node("t/read", fn = function(path) list(v = as.numeric(readLines(path))),
              description = "Lê um número do arquivo indicado pelo caminho.",
              outputs = list(out = "t/box"), params = list(path = tr_param_text("")),
              pure = FALSE,
              fingerprint = function(params) {
                i <- file.info(params$path)
                paste0(params$path, ":", i$size, ":", i$mtime)
              })
    ))
}

store_registry <- function() { r <- tr_registry(); tr_use(store_collection(), registry = r); r }
# Raiz com metacaracteres de REGEX de propósito: exercita, em toda a suíte, o
# caminho que quebrava quando `store$root` era usado como padrão de regex.
tmp_store <- function() tr_store(file.path(tempfile("tr(st)+ore"), "s"))

build <- function(reg, ops) {
  doc <- tr_doc()
  for (op in ops) doc <- tr_doc_apply(doc, op, reg)
  doc
}

# Executor assíncrono FALSO: resolve depois de `ticks` chamadas a collect().
# É o que permite testar cancelamento, adoção e progresso sem daemon.
fake_async_executor <- function(ticks = 2L, capacity = 2L, on_run = NULL) {
  log <- new.env(parent = emptyenv()); log$submitted <- character(); log$cancelled <- character()
  structure(list(
    kind = "fake", log = log,
    capacity = function() capacity,
    # `ctx_extra` guardado no token e devolvido no `collect()`: um executor falso
    # que o engolisse esconderia, de todo teste que passa por ele, exatamente o
    # defeito da Tarefa 5.4 (executor que recebe o ajuste do run e não o repassa).
    submit = function(unit, registry, store, ctx_extra = NULL) {
      log$submitted <- c(log$submitted, unit$node)
      tok <- new.env(parent = emptyenv()); tok$left <- ticks; tok$unit <- unit
      tok$registry <- registry; tok$store <- store; tok$dead <- FALSE
      tok$ctx_extra <- ctx_extra
      tok
    },
    collect = function(tok) {
      if (tok$dead) return(list(ok = FALSE, error = list(message = "cancelado", class = "tr_error_cancelled")))
      tok$left <- tok$left - 1L
      if (tok$left > 0L) return(NULL)
      if (!is.null(on_run)) on_run(tok$unit)
      .tr_capture_unit(tok$unit, tok$registry, tok$store, tok$ctx_extra)
    },
    cancel = function(toks) { for (t in toks) { t$dead <- TRUE; log$cancelled <- c(log$cancelled, t$unit$node) }; invisible(TRUE) },
    shutdown = function() invisible(TRUE)
  ), class = "tr_executor")
}
