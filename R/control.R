#' Canal de controle: a trama aberta no editor, dirigida de fora.
#'
#' Um agente (Claude Code, Codex, script, modelo via API) lê e edita o fluxo
#' que está na TELA. Não é um segundo caminho de escrita: toda op passa pelo
#' mesmo `aplicar()` da sessão Shiny — `tr_submit`, log de undo, autosave,
#' eco pro front e re-execução. O humano vê cada passo e desfaz com Ctrl+Z.
#'
#' O servidor é um `httpuv` irmão, no MESMO processo e no mesmo loop do
#' `later` que o Shiny usa: o handler roda entre dois ciclos do Shiny, nunca
#' em paralelo com um observer, então não há corrida com as ops do front. Fica
#' preso a 127.0.0.1 e exige o token gravado em `.trama/control.json`.
#'
#' Revisão defasada não é problema do agente: sem `base_rev`, a op vale sobre
#' a revisão atual — o que é o rebase, já que aqui nada roda concorrente. Quem
#' quer a proteção estrita manda `base_rev`.
#' @noRd
.tr_control <- new.env(parent = emptyenv())
.tr_control$sessoes <- list()
.tr_control$servidor <- NULL

#' Sessão que responde ao canal: a mais recente que ficou pronta. Abas antigas
#' continuam editando pelo próprio front; o agente fala com a última aberta.
#' @noRd
.tr_control_sessao <- function() {
  s <- .tr_control$sessoes
  if (!length(s)) rlang::abort("Nenhum editor aberto neste processo.", class = "tr_error_control")
  s[[length(s)]]
}

.tr_control_registrar <- function(token, sessao) {
  .tr_control$sessoes[[token]] <- sessao
  .tr_control_arquivo(sessao)
  invisible()
}

.tr_control_remover <- function(token) {
  .tr_control$sessoes[[token]] <- NULL
  invisible()
}

#' Onde o CLI acha porta e token: na pasta do projeto aberto (o agente
#' costuma estar nela) e num lugar fixo do usuário (quando não está).
#' @noRd
.tr_control_caminhos <- function(raiz = NULL) {
  c(if (!is.null(raiz)) file.path(raiz, ".trama", "control-agente.json"),
    file.path(tools::R_user_dir("trama", "cache"), "control-agente.json"))
}

.tr_control_arquivo <- function(sessao = NULL, raiz = NULL) {
  srv <- .tr_control$servidor
  if (is.null(srv)) return(invisible())
  raiz <- if (is.null(sessao)) raiz else shiny::isolate(sessao$raiz())
  info <- list(port = srv$port, token = srv$token, pid = Sys.getpid(), root = raiz)
  # umask ANTES de criar: gravar e só depois dar chmod deixa o token legível
  # por outros usuários no intervalo.
  velho <- Sys.umask("077"); on.exit(Sys.umask(velho), add = TRUE)
  for (p in .tr_control_caminhos(info$root)) {
    tryCatch({
      dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(info, p, auto_unbox = TRUE)
      Sys.chmod(p, "0600")
    }, error = function(e) NULL)
  }
  invisible()
}

#' Sobe o servidor de controle. Idempotente por processo.
#' @noRd
tr_control_start <- function(raiz = NULL, port = NULL) {
  if (!is.null(.tr_control$servidor)) return(invisible(.tr_control$servidor))
  port <- port %||% tr_port_free(tr_port_default() + 1L)
  token <- .tr_entropy_hex(32L)
  h <- httpuv::startServer("127.0.0.1", port,
                           list(call = function(req) .tr_control_http(req, token)))
  .tr_control$servidor <- list(port = port, token = token, handle = h)
  # Grava JÁ, não só quando uma aba abrir: um arquivo de processo anterior
  # (morto sem `onStop`) apontaria o CLI para o token errado.
  .tr_control_arquivo(raiz = raiz)
  invisible(.tr_control$servidor)
}

tr_control_stop <- function() {
  srv <- .tr_control$servidor
  if (is.null(srv)) return(invisible())
  try(httpuv::stopServer(srv$handle), silent = TRUE)
  raizes <- vapply(.tr_control$sessoes, function(s) shiny::isolate(s$raiz()), "")
  for (p in unique(unlist(lapply(c(list(NULL), as.list(raizes)), .tr_control_caminhos)))) {
    info <- tryCatch(jsonlite::read_json(p), error = function(e) NULL)
    if (identical(info$token, srv$token)) unlink(p)
  }
  .tr_control$servidor <- NULL
  .tr_control$sessoes <- list()
  invisible()
}

# HTTP --------------------------------------------------------------------

.tr_control_resposta <- function(status, corpo) {
  list(status = status, headers = list("Content-Type" = "application/json; charset=utf-8"),
       body = as.character(jsonlite::toJSON(corpo, auto_unbox = TRUE, null = "null",
                                            na = "null", digits = NA)))
}

.tr_control_http <- function(req, token) {
  # O token vai em cabeçalho, nunca na URL: URL acaba em log e histórico.
  if (!identical(req$HTTP_AUTHORIZATION, paste("Bearer", token))) {
    return(.tr_control_resposta(401L, list(ok = FALSE, reason = "unauthorized",
      message = "Token errado: o arquivo de controle é de um editor que já fechou. Reabra o editor.")))
  }
  corpo <- NULL
  if (identical(req$REQUEST_METHOD, "POST")) {
    txt <- rawToChar(req$rook.input$read())
    Encoding(txt) <- "UTF-8"
    corpo <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(corpo)) {
      return(.tr_control_resposta(400L, list(ok = FALSE, reason = "malformed",
                                            message = "Corpo não é JSON.")))
    }
  }
  q <- shiny::parseQueryString(req$QUERY_STRING %||% "")
  rota <- paste(req$REQUEST_METHOD, req$PATH_INFO)
  tryCatch({
    res <- switch(rota,
      "GET /state"   = tr_control_state(),
      "GET /catalog" = tr_control_catalog(q$tipo, q$busca),
      "GET /result"  = tr_control_result(q$node),
      "POST /op"     = tr_control_op(corpo$op, corpo$base_rev),
      "POST /cmd"    = tr_control_cmd(corpo),
      "POST /undo"   = tr_control_undo(),
      list(ok = FALSE, reason = "not_found", message = paste("Rota desconhecida:", rota)))
    .tr_control_resposta(if (identical(res$reason, "not_found")) 404L else 200L, res)
  }, error = function(e) {
    .tr_control_resposta(200L, list(ok = FALSE, reason = "rejected", message = conditionMessage(e)))
  })
}

# Verbos ------------------------------------------------------------------

#' Roda `f` como se fosse um observer da sessão: `reactiveVal` só se lê
#' dentro de contexto, e `sendCustomMessage` precisa do domínio certo.
#' @noRd
.tr_control_na_sessao <- function(s, f) {
  shiny::withReactiveDomain(s$session, shiny::isolate(f()))
}

tr_control_state <- function() {
  s <- .tr_control_sessao()
  .tr_control_na_sessao(s, function() {
    doc <- s$doc(); reg <- s$registry()
    nos <- lapply(names(doc$nodes), function(id) {
      n <- doc$nodes[[id]]; st <- s$estado[[id]]
      # Nomeada mesmo vazia: sem nomes, `list()` vira `[]` no JSON, não `{}`.
      params <- if (length(n$params)) n$params else stats::setNames(list(), character())
      list(id = id, type = n$type, label = n$label, params = params,
           status = st$status %||% "idle", message = st$message)
    })
    nos <- .tr_control_com_causa(nos, doc$edges)
    arestas <- lapply(doc$edges, function(e) {
      paste0(e$from$node, ":", e$from$port, " -> ", e$to$node, ":", e$to$port)
    })
    list(ok = TRUE, root = s$raiz(), flow = s$fluxo(), rev = doc$rev,
         nodes = nos, edges = arestas, problems = tr_doc_validate(doc, reg))
  })
}

#' Catálogo enxuto: o bastante para montar um fluxo sem abrir a doc de cada
#' bloco. Com `tipo`, o bloco inteiro como o editor o vê. Com `busca`, só os
#' blocos que casam, do melhor para o pior (o catálogo inteiro passa de 50 KB,
#' caro demais para o agente ler a cada pergunta).
#' @noRd
tr_control_catalog <- function(tipo = NULL, busca = NULL, limite = 12L) {
  s <- .tr_control_sessao()
  .tr_catalogo_enxuto(tr_catalog(shiny::isolate(s$registry())), tipo, busca, limite)
}

#' Corpo de `tr_control_catalog()` sobre um catálogo já montado: o CLI
#' offline usa o mesmo recorte sem editor aberto.
#' @noRd
.tr_catalogo_enxuto <- function(cat, tipo = NULL, busca = NULL, limite = 12L) {
  nos <- cat$nodes %||% cat
  if (!is.null(tipo)) {
    achado <- Filter(function(n) identical(n$id %||% n$type, tipo), nos)
    if (!length(achado)) rlang::abort(sprintf("Bloco desconhecido: '%s'.", tipo))
    return(list(ok = TRUE, node = achado[[1]]))
  }
  portas <- function(ps) vapply(ps %||% list(), function(p) paste0(p$name, ":", p$type), "")
  if (!is.null(busca)) {
    pontos <- vapply(nos, function(n) {
      .tr_busca_ponto(busca, paste(n$label %||% "", n$description %||% "", n$id %||% n$type))
    }, 0)
    # Letras em sequência (1 ponto por palavra) numa descrição longa casam
    # quase tudo. Havendo casamento de verdade, esse ruído sai.
    n_palavras <- length(strsplit(trimws(busca), "\\s+")[[1]])
    minimo <- if (length(pontos) && max(pontos) > n_palavras) n_palavras + 1 else 1
    ordem <- order(-pontos, seq_along(nos))
    nos <- nos[ordem[pontos[ordem] >= minimo]]
    total <- length(nos)
    nos <- utils::head(nos, limite)
  }
  list(ok = TRUE, total = if (!is.null(busca)) total, nodes = lapply(nos, function(n) list(
    type = n$id %||% n$type, label = n$label, description = n$description,
    inputs = portas(n$inputs), outputs = portas(n$outputs),
    params = vapply(n$params %||% list(), function(p) p$name %||% "", "")
  )))
}

tr_control_op <- function(op, base_rev = NULL) {
  if (is.null(op)) rlang::abort("Falta o campo 'op'.")
  s <- .tr_control_sessao()
  .tr_control_na_sessao(s, function() {
    env <- list(seq = paste0("agente-", .tr_entropy_hex(6L)),
                base_rev = base_rev %||% s$doc()$rev, op = op, autor = "agente")
    res <- s$aplicar(env)
    list(ok = isTRUE(res$ok), rev = s$doc()$rev, op = res$op, reason = res$reason,
         message = res$message)
  })
}

tr_control_undo <- function() {
  s <- .tr_control_sessao()
  .tr_control_na_sessao(s, function() { s$desfazer(); list(ok = TRUE, rev = s$doc()$rev) })
}

#' Açúcar do CLI: `add`, `link`, `set`, `rm`. Monta as ops com a mesma DSL de
#' `tr_add()`/`tr_link()` (porta omitida = primeira compatível) sobre o
#' documento atual, e manda tudo como UM batch — um Ctrl+Z desfaz o gesto.
#' @noRd
tr_control_cmd <- function(m) {
  s <- .tr_control_sessao()
  ops <- .tr_control_na_sessao(s, function() {
    fl <- tr_flow(s$registry(), s$doc()); fl$ops <- list()
    fl <- switch(m$cmd %||% "",
      add  = .tr_control_add(fl, m),
      link = tr_link(fl, m$from, m$to),
      set  = do.call(tr_set, c(list(fl, m$node), m$params)),
      rm   = .tr_flow_apply(fl, list(op = "remove_node", node = m$node)),
      rlang::abort(sprintf("Comando desconhecido: '%s'.", m$cmd %||% "")))
    fl$ops
  })
  op <- if (length(ops) == 1L) ops[[1]] else list(op = "batch", ops = ops)
  tr_control_op(op)
}

.tr_control_add <- function(fl, m) {
  doc <- fl$doc
  id <- m$id %||% .tr_new_id()
  pos <- doc$ui$positions
  origem <- if (length(m$from)) .tr_split_ref(m$from[[1]])$node
  xs <- vapply(pos, function(p) as.numeric(p[[1]]), 0)
  larguras <- vapply(names(pos), function(no) {
    sz <- doc$ui$sizes[[no]]
    if (is.null(sz) || length(sz) < 1L) 300 else as.numeric(sz[[1]])
  }, 0)
  # Altura de gráfico/tabela pode crescer depois do run; sem saber sua altura
  # futura, a garantia geométrica é abrir coluna após todos os cards. Continua
  # alinhando com a origem para manter o filho fácil de localizar.
  if (!is.null(m$position)) {
    p <- unlist(m$position)
  } else {
    x <- if (length(xs)) max(xs + larguras) + 60 else 0
    y <- if (!is.null(origem) && !is.null(pos[[origem]])) pos[[origem]][[2]] else 0
    p <- c(x, y)
  }
  do.call(tr_add, c(list(fl, id, m$type), m$params %||% list(),
                    list(from = if (length(m$from)) unlist(m$from), label = m$label,
                         position = p)))
}

#' O que o nó produziu: status, erro, e o que o store já guarda de cada
#' saída (resumo, schema, preview). Arquivo de preview (PNG, SVG) volta com
#' caminho ABSOLUTO — o agente abre a imagem. Não dispara execução: nó que
#' não rodou diz que não rodou.
#' @noRd
tr_control_result <- function(node) {
  if (is.null(node)) rlang::abort("Falta 'node'.")
  s <- .tr_control_sessao()
  .tr_control_na_sessao(s, function() {
    if (is.null(s$doc()$nodes[[node]])) rlang::abort(sprintf("Nó desconhecido: '%s'.", node))
    st <- s$estado[[node]]
    if (is.null(st)) {
      return(list(ok = TRUE, node = node, status = "idle",
                  message = "Ainda não rodou nesta sessão."))
    }
    store <- s$store()
    saidas <- lapply(st$handles %||% list(), function(h) {
      if (is.character(h)) h <- tryCatch(tr_store_handle(store, h), error = function(e) NULL)
      if (is.null(h)) return(NULL)
      pv <- h$preview
      if (!is.null(pv$files)) pv$files <- lapply(pv$files, function(f) file.path(store$root, f))
      list(type = h$type, summary = h$summary, schema = h$schema, preview = pv)
    })
    causa <- if (identical(st$status, "blocked")) {
      status <- vapply(names(s$doc()$nodes), function(id) s$estado[[id]]$status %||% "idle", "")
      .tr_control_causa(node, status, s$doc()$edges)
    }
    list(ok = TRUE, node = node, status = st$status, message = st$message,
         causa = causa, outputs = saidas)
  })
}

# Diagnóstico -------------------------------------------------------------

#' Por que um nó está `blocked`: sobe pelas arestas até os ancestrais que
#' falharam (`failed`/`invalid`), sem passar deles. O card bloqueado não tem
#' mensagem própria; sem isto o agente teria de caçar o culpado nó a nó.
#' @param status Vetor nomeado id -> status.
#' @return Lista de `list(node, status)`, vazia se nada acima falhou.
#' @noRd
.tr_control_causa <- function(node, status, edges) {
  pais <- function(id) unique(vapply(Filter(function(e) identical(e$to$node, id), edges),
                                     function(e) e$from$node, ""))
  visto <- character(); fila <- pais(node); achados <- character()
  while (length(fila)) {
    id <- fila[[1]]; fila <- fila[-1]
    if (id %in% visto) next
    visto <- c(visto, id)
    if ((status[[id]] %||% "idle") %in% c("failed", "invalid")) achados <- c(achados, id)
    else fila <- c(fila, pais(id))
  }
  lapply(achados, function(id) list(node = id, status = status[[id]]))
}

.tr_control_com_causa <- function(nos, edges) {
  status <- stats::setNames(vapply(nos, function(n) n$status, ""), vapply(nos, function(n) n$id, ""))
  lapply(nos, function(n) {
    if (identical(n$status, "blocked")) n$causa <- .tr_control_causa(n$id, status, edges)
    n
  })
}

#' Busca aproximada, a mesma do "+" do editor (`pontoBusca` em
#' `inst/www/fantasmas.js`): sem acento nem caixa, toda palavra do termo tem
#' de casar. Início de palavra vale 3, trecho no meio 2, letras em sequência 1
#' ("anv" acha "ANOVA"); uma palavra que não casa zera o bloco.
#' @noRd
.tr_busca_ponto <- function(termo, texto) {
  norm <- function(x) tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  qs <- strsplit(trimws(norm(termo)), "\\s+")[[1]]
  qs <- qs[nzchar(qs)]
  if (!length(qs)) return(0)
  t <- norm(texto)
  palavras <- strsplit(t, "[^a-z0-9]+")[[1]]
  palavras <- palavras[nzchar(palavras)]
  subseq <- function(q, w) {
    cs <- strsplit(q, "")[[1]]; i <- 1L
    for (c in strsplit(w, "")[[1]]) if (c == cs[[i]]) { i <- i + 1L; if (i > length(cs)) return(TRUE) }
    FALSE
  }
  total <- 0
  for (q in qs) {
    total <- total + if (any(startsWith(palavras, q))) 3
      else if (grepl(q, t, fixed = TRUE)) 2
      else if (any(vapply(palavras, subseq, TRUE, q = q)) || subseq(q, gsub("\\s+", "", t))) 1
      else return(0)
  }
  total
}
