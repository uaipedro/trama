#' Linha de comando do canal de controle.
#'
#' Fala com o editor aberto (`tr_app()`) pelo servidor de `R/control.R`. Toda
#' saída é JSON em stdout, para agente ler; recusa sai com `ok: false` e,
#' pelo script `trama-agente`, status 1.
#'
#' ```
#' trama-agente state                       # fluxo na tela: nós, status, arestas
#' trama-agente catalog [tipo]              # blocos disponíveis / um bloco inteiro
#' trama-agente add <tipo> [--id X] [--from no[:porta]]... [--label L] [param=valor]...
#' trama-agente link <no[:porta]> <no[:porta]>
#' trama-agente set <no> param=valor...
#' trama-agente rm <no>
#' trama-agente op '<json>'                 # op crua (ver schema do documento)
#' trama-agente apply <arquivo.json>        # lista de ops como um passo de undo
#' trama-agente result <no> [--wait 30]     # status, resumo, preview, PNG
#' trama-agente undo
#' ```
#'
#' `valor` é lido como JSON quando dá (`n=3`, `x=true`, `v=["a","b"]`) e como
#' texto quando não (`coluna=peso`). `--projeto DIR` escolhe qual editor
#' quando há mais de um.
#' @param args Vetor de argumentos, como `commandArgs(TRUE)`.
#' @return A resposta do editor (lista), invisível; imprime o JSON.
#' @export
tr_cli <- function(args = commandArgs(TRUE)) {
  a <- .tr_cli_parse(args)
  res <- tryCatch(.tr_cli_run(a), error = function(e) {
    list(ok = FALSE, reason = "cli", message = conditionMessage(e))
  })
  cat(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null", na = "null",
                       digits = NA, pretty = TRUE), "\n")
  invisible(res)
}

.tr_cli_uso <- c(
  "state                                   fluxo na tela: nós, params, status, ligações",
  "catalog [tipo]                          blocos disponíveis; com tipo, params/escolhas/ajuda",
  "add <tipo> [--id X] [--from no[:porta]]... [--label L] [param=valor]...",
  "link <no[:porta]> <no[:porta]>          porta omitida = primeira compatível",
  "set <no> param=valor...",
  "rm <no>",
  "op '<json>'                             op crua do documento",
  "apply <arquivo.json>                    lista de ops como um passo de desfazer",
  "result <no> [--wait segundos]           status, resumo, preview, PNG",
  "undo                                    desfaz a última edição (sua ou do humano)",
  "valor: JSON quando dá (n=3, x=true, 'cols=[\"a\",\"b\"]'), senão texto.",
  "--projeto DIR escolhe o editor quando há mais de um.")

.tr_cli_parse <- function(args) {
  pos <- character(); opt <- list(from = character())
  i <- 1L
  while (i <= length(args)) {
    x <- args[[i]]
    if (startsWith(x, "--")) {
      nm <- substring(x, 3L)
      if (i == length(args)) rlang::abort(sprintf("'%s' sem valor.", x))
      opt[[nm]] <- c(if (nm == "from") opt$from, args[[i + 1L]])
      i <- i + 2L
    } else {
      pos <- c(pos, x); i <- i + 1L
    }
  }
  list(cmd = pos[1], pos = pos[-1], opt = opt)
}

.tr_cli_valor <- function(v) {
  tryCatch(jsonlite::fromJSON(v, simplifyVector = FALSE), error = function(e) v)
}

.tr_cli_params <- function(xs) {
  kv <- xs[grepl("=", xs, fixed = TRUE)]
  if (length(kv) != length(xs)) {
    rlang::abort(sprintf("Esperava param=valor, veio: %s.", paste(setdiff(xs, kv), collapse = " ")))
  }
  nomes <- sub("=.*$", "", kv)
  stats::setNames(lapply(sub("^[^=]*=", "", kv), .tr_cli_valor), nomes)
}

.tr_cli_run <- function(a) {
  p <- a$pos
  precisa <- function(n, uso) if (length(p) < n) rlang::abort(paste("Uso:", uso))
  if (is.null(a$cmd) || is.na(a$cmd) || a$cmd %in% c("help", "ajuda", "-h")) {
    return(list(ok = TRUE, uso = .tr_cli_uso))
  }
  cx <- .tr_cli_conexao(a$opt$projeto)
  switch(a$cmd,
    state   = .tr_cli_http(cx, "GET", "/state"),
    catalog = .tr_cli_http(cx, "GET", "/catalog", query = if (length(p)) list(tipo = p[[1]])),
    add = {
      precisa(1, "trama add <tipo> [--id X] [--from no] [param=valor]...")
      .tr_cli_http(cx, "POST", "/cmd", list(
        cmd = "add", type = p[[1]], id = a$opt$id, label = a$opt$label,
        from = if (length(a$opt$from)) as.list(a$opt$from),
        params = .tr_cli_params(p[-1])))
    },
    link = { precisa(2, "trama link <no[:porta]> <no[:porta]>")
      .tr_cli_http(cx, "POST", "/cmd", list(cmd = "link", from = p[[1]], to = p[[2]])) },
    set = { precisa(2, "trama set <no> param=valor...")
      .tr_cli_http(cx, "POST", "/cmd", list(cmd = "set", node = p[[1]],
                                             params = .tr_cli_params(p[-1]))) },
    rm = { precisa(1, "trama rm <no>")
      .tr_cli_http(cx, "POST", "/cmd", list(cmd = "rm", node = p[[1]])) },
    op = { precisa(1, "trama op '<json>'")
      .tr_cli_http(cx, "POST", "/op", list(op = jsonlite::fromJSON(p[[1]], simplifyVector = FALSE))) },
    apply = {
      precisa(1, "trama apply <arquivo.json>")
      ops <- jsonlite::read_json(p[[1]], simplifyVector = FALSE)
      op <- if (!is.null(ops$op)) ops else list(op = "batch", ops = ops)
      .tr_cli_http(cx, "POST", "/op", list(op = op))
    },
    result = { precisa(1, "trama result <no> [--wait segundos]")
      .tr_cli_result(cx, p[[1]], as.numeric(a$opt$wait %||% 0)) },
    undo = .tr_cli_http(cx, "POST", "/undo", list()),
    rlang::abort(paste0("Comando desconhecido: '", a$cmd, "'. Veja 'trama-agente help'.")))
}

#' Espera o nó sair de "na fila/rodando". Execução é assíncrona: a op volta
#' assim que aplicada, e o resultado chega depois.
#' @noRd
.tr_cli_result <- function(cx, node, wait) {
  fim <- Sys.time() + wait
  repeat {
    r <- .tr_cli_http(cx, "GET", "/result", query = list(node = node))
    if (!isTRUE(r$ok) || !(r$status %in% c("running", "idle", "queued")) || Sys.time() >= fim) return(r)
    Sys.sleep(0.3)
  }
}

#' Acha o editor: `--projeto`, senão subindo a partir da pasta atual até um
#' `.trama/control-agente.json`, senão o arquivo global do usuário.
#' @noRd
.tr_cli_conexao <- function(projeto = NULL) {
  cands <- if (!is.null(projeto)) {
    file.path(normalizePath(projeto, mustWork = FALSE), ".trama", "control-agente.json")
  } else {
    d <- normalizePath(getwd()); acima <- character()
    repeat {
      acima <- c(acima, file.path(d, ".trama", "control-agente.json"))
      pai <- dirname(d); if (identical(pai, d)) break; d <- pai
    }
    c(acima, .tr_control_caminhos())
  }
  for (f in cands) if (file.exists(f)) return(jsonlite::read_json(f))
  rlang::abort("Nenhum editor aberto encontrado. Suba com trama::tr_app() e tente de novo.")
}

.tr_cli_http <- function(cx, metodo, rota, corpo = NULL, query = NULL) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    rlang::abort("O CLI precisa do pacote 'curl': install.packages('curl').")
  }
  url <- sprintf("http://127.0.0.1:%s%s", cx$port, rota)
  if (length(query)) {
    url <- paste0(url, "?", paste(names(query), vapply(query, curl::curl_escape, ""),
                                  sep = "=", collapse = "&"))
  }
  h <- curl::new_handle(timeout = 60)
  curl::handle_setheaders(h, Authorization = paste("Bearer", cx$token),
                          `Content-Type` = "application/json")
  if (identical(metodo, "POST")) {
    curl::handle_setopt(h, customrequest = "POST", postfields = as.character(
      jsonlite::toJSON(corpo, auto_unbox = TRUE, null = "null", digits = NA)))
  }
  r <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) {
    rlang::abort(paste0("Editor não respondeu (", conditionMessage(e), "). ",
                        "Se ele está rodando algo pesado, espere e tente de novo."))
  })
  jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
}
