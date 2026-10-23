#' Linha de comando do canal de controle.
#'
#' Fala com o editor aberto (`tr_app()`) pelo servidor de `R/control.R`. Toda
#' saída é JSON em stdout, para agente ler, inclusive `help` e erro de
#' argumento; recusa sai com `ok: false` e, pelo script `trama-agente`,
#' status 1.
#'
#' ```
#' trama-agente state                       # fluxo na tela: nós, status, arestas
#' trama-agente catalog [tipo] [--busca T]  # blocos disponíveis / um bloco / os que casam
#' trama-agente add <tipo> [--id X] [--from no[:porta]]... [--label L] [param=valor]...
#' trama-agente link <no[:porta]> <no[:porta]>
#' trama-agente set <no> param=valor...
#' trama-agente rm <no>
#' trama-agente op '<json>'                 # op crua (ver schema do documento)
#' trama-agente apply <arquivo.json>        # lista de ops como um passo de undo
#' trama-agente result <no> [--wait 30]     # status, resumo, preview, PNG
#' trama-agente undo
#' trama-agente accept [no]...              # aceita a versão nova (version_drift)
#' trama-agente rename <no> <rótulo>
#' trama-agente move <id>=<x>,<y>...        # ou: move <id>... --dx N --dy N
#' trama-agente frame add "<título>" [no]... [--id F] [--color C] [--aspect A]
#' trama-agente frame set|rm|put|take <frame> ...
#' trama-agente note add "<markdown>" [--frame F | --node N] [--id X] [--file nota.md]
#' trama-agente note set|rm <nota> ...
#' trama-agente layout                      # auto-layout por etapas (frames)
#' trama-agente guide                       # sem editor: como contar a análise
#' trama-agente explain <tipo>              # sem editor: o bloco inteiro
#' trama-agente validate <arquivo.json>     # sem editor: lê, migra e valida
#' ```
#'
#' `catalog`, `explain` e `validate` funcionam sem editor aberto, sobre as
#' coleções instaladas (ou `--colecoes a,b`). `catalog` só faz isso quando
#' não acha editor ou com `--offline`.
#'
#' Toda edição (`add`, `link`, `set`, `rm`, `op`, `apply`, `undo`, `accept`) aceita
#' `--wait N`: espera até N segundos o fluxo parar de rodar e devolve, em
#' `efeito`, o status dos nós tocados e de tudo abaixo deles. Nó `blocked`
#' traz `causa`, os ancestrais que falharam.
#'
#' `valor` é lido como JSON quando dá (`n=3`, `x=true`, `v=["a","b"]`) e como
#' texto quando não (`coluna=peso`). `--projeto DIR` escolhe qual editor
#' quando há mais de um.
#' @param args Vetor de argumentos, como `commandArgs(TRUE)`.
#' @return A resposta do editor (lista), invisível; imprime o JSON.
#' @export
tr_cli <- function(args = commandArgs(TRUE)) {
  # Nenhum erro escapa do envelope: argumento malformado vira
  # `reason: "args"`, falha adiante vira `reason: "cli"`. Agente lê stdout
  # como JSON sempre, sem backtrace de R no meio.
  a <- tryCatch(.tr_cli_parse(args), error = function(e) {
    list(erro = list(ok = FALSE, reason = "args", message = conditionMessage(e)))
  })
  res <- if (!is.null(a$erro)) a$erro else tryCatch(.tr_cli_run(a), error = function(e) {
    list(ok = FALSE, reason = "cli", message = conditionMessage(e))
  })
  cat(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null", na = "null",
                       digits = NA, pretty = TRUE), "\n")
  invisible(res)
}

.tr_cli_uso <- c(
  "state                                   fluxo na tela: nós, params, status, ligações",
  "catalog [tipo] [--busca termo]          blocos disponíveis; com tipo, params/escolhas/ajuda",
  "add <tipo> [--id X] [--from no[:porta]]... [--label L] [param=valor]...",
  "link <no[:porta]> <no[:porta]>          porta omitida = primeira compatível",
  "set <no> param=valor...",
  "rm <no>",
  "op '<json>'                             op crua do documento",
  "apply <arquivo.json>                    lista de ops como um passo de desfazer",
  "result <no> [--wait segundos]           status, resumo, preview, PNG",
  "undo                                    desfaz a última edição (sua ou do humano)",
  "accept [no]...                          aceita a versão nova dos blocos em version_drift (sem no: todos)",
  "--- narrativa: frames, notas, posição (veja 'guide') ---",
  "add ... --frame F                       o bloco novo já entra no frame F",
  "rename <no> <rótulo>                    muda o rótulo do card (legenda no documento)",
  "move <id>=<x>,<y>...                    posição absoluta de blocos, frames ou notas",
  "move <id>... --dx N --dy N              desloca; frame leva o que tem dentro, grupo anda junto",
  "frame add \"<título>\" [no]... [--id F] [--color C] [--aspect livre]   frame novo à direita, com os blocos dentro",
  "frame set <frame> [--title T] [--color C] [--aspect A] [--order N] [--w N --h N]",
  "frame put|take <frame> <no>...          põe ou tira blocos (o frame se rearruma)",
  "frame rm <frame>                        tira o frame; os blocos ficam",
  "note add \"<markdown>\" [--frame F | --node N] [--id X] [--color C] [--w N] [--h N] [--file nota.md]",
  "note set <nota> [--text T | --file nota.md] [--color C] [--w N] [--h N] [--x N --y N]",
  "note rm <nota>",
  "layout                                  auto-layout por etapas: um frame por coluna, diagnósticos abaixo",
  "cores: azul verde amarelo laranja rosa roxo cinza (nota também: nenhuma)",
  "--- sem editor aberto ---",
  "guide                                   como contar a análise como narrativa (frames, notas, layout)",
  "catalog ... [--offline]                 sem editor (ou com --offline), lê das coleções instaladas",
  "explain <tipo>                          bloco inteiro: params, portas, ajuda, referências",
  "validate <arquivo.json>                 lê, migra e valida um fluxo ou template",
  "--colecoes a,b escolhe as coleções do modo sem editor (padrão: todas as trama.* instaladas).",
  "--wait N em qualquer edição: espera rodar e devolve 'efeito' (status abaixo, causa do bloqueio).",
  "valor: JSON quando dá (n=3, x=true, 'cols=[\"a\",\"b\"]'), senão texto.",
  "--projeto DIR escolhe o editor quando há mais de um.")

# Opções sem valor. `--json` é aceita e ignorada: a saída já é sempre JSON.
.tr_cli_bandeiras <- c("json", "offline")

.tr_cli_parse <- function(args) {
  pos <- character(); opt <- list(from = character())
  i <- 1L
  while (i <= length(args)) {
    x <- args[[i]]
    if (startsWith(x, "--")) {
      nm <- substring(x, 3L)
      if (nm %in% .tr_cli_bandeiras) { opt[[nm]] <- TRUE; i <- i + 1L; next }
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
    return(list(ok = TRUE, comando = "trama-agente <comando> [args]", uso = .tr_cli_uso))
  }
  if (a$cmd %in% .tr_cli_offline || (a$cmd == "catalog" && isTRUE(a$opt$offline))) {
    return(.tr_cli_sem_editor(a, p, precisa))
  }
  cx <- tryCatch(.tr_cli_conexao(a$opt$projeto), error = function(e) {
    if (a$cmd == "catalog") NULL else stop(e)
  })
  if (is.null(cx)) return(.tr_cli_sem_editor(a, p, precisa))
  if (a$cmd == "catalog") {
    # Arquivo de controle velho (editor fechado sem limpar) também cai no
    # registro local: catálogo é leitura, repetir não custa nada.
    return(tryCatch(.tr_cli_despachar(a, cx, p, precisa),
                    error = function(e) .tr_cli_sem_editor(a, p, precisa)))
  }
  res <- .tr_cli_despachar(a, cx, p, precisa)
  espera <- as.numeric(a$opt$wait %||% 0)
  if (a$cmd %in% .tr_cli_edicoes && isTRUE(res$ok) && espera > 0) {
    res$efeito <- .tr_cli_efeito(cx, .tr_cli_tocados(res$op), espera)
  }
  res
}

# Comandos que não precisam do editor. `catalog` só cai aqui sem editor
# aberto ou com `--offline`; `validate` e `explain` sempre.
.tr_cli_offline <- c("validate", "explain", "guide")

.tr_cli_sem_editor <- function(a, p, precisa, registro = NULL) {
  reg <- function() registro %||% .tr_cli_registro(a$opt$colecoes)
  switch(a$cmd,
    guide = list(ok = TRUE, guia = .tr_guia_narrativa),
    catalog = c(.tr_catalogo_enxuto(tr_catalog(reg()), tipo = if (length(p)) p[[1]],
                                    busca = a$opt$busca),
                offline = TRUE),
    explain = {
      precisa(1, "trama explain <tipo>")
      c(.tr_catalogo_enxuto(tr_catalog(reg()), tipo = p[[1]]), offline = TRUE)
    },
    validate = {
      precisa(1, "trama validate <arquivo.json>")
      .tr_cli_validate(p[[1]], a$opt$colecoes, registro)
    })
}

#' Registro sem editor: as coleções pedidas (`--colecoes a,b`) ou todas as
#' `trama.*` carregadas ou instaladas. Coleção que não carrega fica de fora
#' com aviso no stderr, em vez de derrubar o comando.
#' @noRd
.tr_cli_registro <- function(colecoes = NULL) {
  pkgs <- if (length(colecoes)) trimws(strsplit(colecoes, ",", fixed = TRUE)[[1]]) else {
    .tr_colecoes_instaladas(union(loadedNamespaces(), rownames(utils::installed.packages())))
  }
  reg <- tr_registry()
  for (p in pkgs) {
    if (p %in% .tr_registry_packages(reg)) next
    tryCatch(tr_use(p, registry = reg), error = function(e) {
      message(sprintf("trama-agente: cole\u00e7\u00e3o '%s' ignorada (%s)", p, conditionMessage(e)))
    })
  }
  reg
}

#' Lê um documento (`.json` de fluxo) ou template, migra e valida. As
#' coleções vêm de `--colecoes`, do template, do `trama.json` do projeto
#' acima do arquivo ou, por fim, de todas as disponíveis.
#' @noRd
.tr_cli_validate <- function(path, colecoes = NULL, registro = NULL) {
  if (!file.exists(path)) rlang::abort(sprintf("Arquivo n\u00e3o encontrado: '%s'.", path))
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  bruto <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  if (identical(bruto$trama, "template")) {
    if (is.null(colecoes) && length(bruto$colecoes)) colecoes <- paste(unlist(bruto$colecoes), collapse = ",")
    txt <- as.character(jsonlite::toJSON(bruto$doc, auto_unbox = TRUE, null = "null", digits = NA))
  }
  if (is.null(colecoes) && is.null(registro)) {
    d <- dirname(normalizePath(path))
    repeat {
      cfg <- file.path(d, "trama.json")
      if (file.exists(cfg)) {
        cols <- unlist(jsonlite::read_json(cfg)$collections)
        if (length(cols)) colecoes <- paste(cols, collapse = ",")
        break
      }
      pai <- dirname(d); if (identical(pai, d)) break; d <- pai
    }
  }
  doc <- tr_doc_parse(txt)
  reg <- registro %||% .tr_cli_registro(colecoes)
  migrado <- tr_doc_migrate(doc, reg)
  problemas <- tr_doc_validate(doc, reg)
  list(ok = !length(problemas), file = path, nodes = length(doc$nodes),
       edges = length(doc$edges), migrated = isTRUE(attr(migrado, "migrated")),
       collections = .tr_registry_packages(reg), problems = problemas)
}

.tr_cli_edicoes <- c("add", "link", "set", "rm", "op", "apply", "undo", "accept")

.tr_cli_despachar <- function(a, cx, p, precisa) {
  switch(a$cmd,
    state   = .tr_cli_http(cx, "GET", "/state"),
    catalog = .tr_cli_http(cx, "GET", "/catalog",
                           query = Filter(Negate(is.null), list(tipo = if (length(p)) p[[1]], busca = a$opt$busca))),
    add = {
      precisa(1, "trama add <tipo> [--id X] [--from no] [param=valor]...")
      .tr_cli_http(cx, "POST", "/cmd", list(
        cmd = "add", type = p[[1]], id = a$opt$id, label = a$opt$label, frame = a$opt$frame,
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
    rename = { precisa(2, "trama rename <no> <r\u00f3tulo>")
      .tr_cli_http(cx, "POST", "/cmd", list(cmd = "rename", node = p[[1]], label = paste(p[-1], collapse = " "))) },
    move = { precisa(1, "trama move <id>=<x>,<y>... | move <id>... --dx N --dy N")
      .tr_cli_http(cx, "POST", "/cmd", list(cmd = "move", items = .tr_cli_itens(p, a$opt))) },
    frame = .tr_cli_http(cx, "POST", "/cmd", .tr_cli_frame(p, a$opt, precisa)),
    note = .tr_cli_http(cx, "POST", "/cmd", .tr_cli_nota(p, a$opt, precisa)),
    layout = .tr_cli_http(cx, "POST", "/cmd", list(cmd = "layout")),
    accept = .tr_cli_http(cx, "POST", "/cmd", list(cmd = "accept", nodes = if (length(p)) as.list(p))),
    rlang::abort(paste0("Comando desconhecido: '", a$cmd, "'. Veja 'trama-agente help'.")))
}

#' `id=x,y` vira posição absoluta; `id` sozinho anda `--dx`/`--dy`.
#' @noRd
.tr_cli_itens <- function(p, opt) {
  lapply(p, function(x) {
    if (grepl("=", x, fixed = TRUE)) {
      xy <- suppressWarnings(as.numeric(strsplit(sub("^[^=]*=", "", x), ",", fixed = TRUE)[[1]]))
      if (length(xy) != 2L || anyNA(xy)) rlang::abort(sprintf("Esperava id=x,y, veio: %s.", x))
      list(id = sub("=.*$", "", x), x = xy[[1]], y = xy[[2]])
    } else {
      if (is.null(opt$dx) && is.null(opt$dy)) {
        rlang::abort(sprintf("'%s' sem posi\u00e7\u00e3o: use %s=x,y ou --dx/--dy.", x, x))
      }
      list(id = x, dx = .tr_cli_num(opt$dx %||% 0, "dx"), dy = .tr_cli_num(opt$dy %||% 0, "dy"))
    }
  })
}

.tr_cli_num <- function(v, nome) {
  if (is.null(v)) return(NULL)
  n <- suppressWarnings(as.numeric(v))
  if (length(n) != 1L || is.na(n)) rlang::abort(sprintf("--%s espera um n\u00famero, veio: %s.", nome, v))
  n
}

.tr_cli_texto <- function(opt, texto = NULL) {
  if (!is.null(opt$file)) {
    if (!file.exists(opt$file)) rlang::abort(sprintf("Arquivo n\u00e3o encontrado: '%s'.", opt$file))
    return(paste(readLines(opt$file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }
  texto
}

.tr_cli_frame <- function(p, opt, precisa) {
  precisa(1, "trama frame add|set|rm|put|take ...")
  acao <- p[[1]]
  num <- function(k) .tr_cli_num(opt[[k]], k)
  if (acao == "add") {
    precisa(2, "trama frame add \"<t\u00edtulo>\" [no]... [--id F] [--color C] [--aspect A]")
    return(Filter(Negate(is.null), list(cmd = "frame", acao = "add", title = p[[2]], id = opt$id,
                                        color = opt$color, aspect = opt$aspect, nodes = as.list(p[-(1:2)]))))
  }
  precisa(2, sprintf("trama frame %s <frame> ...", acao))
  Filter(Negate(is.null), list(cmd = "frame", acao = acao, frame = p[[2]], nodes = as.list(p[-(1:2)]),
                               title = opt$title, color = opt$color, aspect = opt$aspect,
                               order = num("order"), w = num("w"), h = num("h")))
}

.tr_cli_nota <- function(p, opt, precisa) {
  precisa(1, "trama note add|set|rm ...")
  acao <- p[[1]]
  num <- function(k) .tr_cli_num(opt[[k]], k)
  comuns <- list(color = opt$color, w = num("w"), h = num("h"), x = num("x"), y = num("y"),
                 escala = opt$escala, fundo = opt$fundo)
  if (acao == "add") {
    texto <- .tr_cli_texto(opt, if (length(p) >= 2L) paste(p[-1], collapse = " "))
    if (is.null(texto)) rlang::abort("Uso: trama note add \"<markdown>\" [--frame F | --node N] (ou --file nota.md)")
    return(Filter(Negate(is.null), c(list(cmd = "note", acao = "add", text = texto, id = opt$id,
                                           frame = opt$frame, node = opt$node), comuns)))
  }
  precisa(2, sprintf("trama note %s <nota> ...", acao))
  Filter(Negate(is.null), c(list(cmd = "note", acao = acao, note = p[[2]],
                                 text = .tr_cli_texto(opt, opt$text)), comuns))
}

#' Nós que a op mexeu diretamente. `remove_node` e `undo` não deixam nó para
#' apontar: devolvem `NULL`, e o efeito vira o fluxo inteiro.
#' @noRd
.tr_cli_tocados <- function(op) {
  if (is.null(op)) return(NULL)
  switch(op$op %||% "",
    batch = unique(unlist(lapply(op$ops, .tr_cli_tocados))),
    add_node = op$id,
    connect = , disconnect = op$to_node,
    remove_node = NULL,
    op$node)
}

#' Tocados mais tudo abaixo deles, depois que o fluxo parar de rodar.
#' @noRd
.tr_cli_efeito <- function(cx, tocados, espera) {
  fim <- Sys.time() + espera
  repeat {
    st <- .tr_cli_http(cx, "GET", "/state")
    if (!isTRUE(st$ok)) return(NULL)
    nos <- .tr_cli_abaixo(st, tocados)
    rodando <- any(vapply(nos, function(n) n$status %in% c("queued", "running"), TRUE))
    if (!rodando || Sys.time() >= fim) break
    Sys.sleep(0.3)
  }
  lapply(nos, function(n) Filter(Negate(is.null),
    list(node = n$id, status = n$status, message = n$message, causa = if (length(n$causa)) n$causa)))
}

#' Nós de `st$nodes` que são `tocados` ou descendem deles (pelas arestas
#' "de:porta -> para:porta" do state). `tocados` NULL = todos.
#' @noRd
.tr_cli_abaixo <- function(st, tocados) {
  if (is.null(tocados)) return(st$nodes)
  pares <- do.call(rbind, lapply(st$edges, function(e) {
    lados <- strsplit(e, " -> ", fixed = TRUE)[[1]]
    sub(":[^:]*$", "", lados)
  }))
  alcance <- tocados
  repeat {
    novos <- if (is.null(pares)) character() else setdiff(pares[pares[, 1] %in% alcance, 2], alcance)
    if (!length(novos)) break
    alcance <- c(alcance, novos)
  }
  Filter(function(n) n$id %in% alcance, st$nodes)
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
  # O handler do editor compartilha o loop com o executor sequencial. Um
  # timeout finito pode vencer depois de aplicar a op e induzir repetição.
  # timeout=0 no libcurl significa aguardar sem limite até a resposta final.
  h <- curl::new_handle(timeout = 0)
  curl::handle_setheaders(h, Authorization = paste("Bearer", cx$token),
                          `Content-Type` = "application/json")
  if (identical(metodo, "POST")) {
    curl::handle_setopt(h, customrequest = "POST", postfields = as.character(
      jsonlite::toJSON(corpo, auto_unbox = TRUE, null = "null", digits = NA)))
  }
  r <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) {
    rlang::abort(paste0("Não foi possível confirmar a resposta do editor (", conditionMessage(e),"). ",
                        "Consulte trama-agente state antes de repetir a operação."))
  })
  jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)
}
