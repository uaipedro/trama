# Canal de controle (R/control.R): o agente edita a trama da TELA pelo mesmo
# `aplicar()` da sessão. O que se trava aqui: token, rebase sem base_rev, recusa
# estruturada, açúcar `add --from` virando um batch, e `result` sem executar.

projeto_ctl <- function() {
  root <- withr::local_tempdir("ctl", .local_envir = parent.frame())
  tr_project_new(root, character())
  tr_project_at(root, test_registry())
}

limpar_ctl <- function(env = parent.frame()) {
  withr::defer({ .tr_control$sessoes <- list(); .tr_control$servidor <- NULL }, envir = env)
}

test_that("sem token certo a rota recusa com 401 e não toca em nada", {
  req <- list(REQUEST_METHOD = "GET", PATH_INFO = "/state", HTTP_AUTHORIZATION = "Bearer x")
  r <- .tr_control_http(req, token = "certo")
  expect_equal(r$status, 401L)
  expect_match(r$body, "unauthorized")
  r <- .tr_control_http(req[-3], token = "certo")
  expect_equal(r$status, 401L)
})

test_that("rota desconhecida é 404 e JSON malformado é 400", {
  r <- .tr_control_http(list(REQUEST_METHOD = "GET", PATH_INFO = "/nada",
                             HTTP_AUTHORIZATION = "Bearer k"), token = "k")
  expect_equal(r$status, 404L)
  corpo <- list(read = function() charToRaw("{nao json"))
  r <- .tr_control_http(list(REQUEST_METHOD = "POST", PATH_INFO = "/op", rook.input = corpo,
                             HTTP_AUTHORIZATION = "Bearer k"), token = "k")
  expect_equal(r$status, 400L)
})

test_that("sem editor aberto, o canal diz isso em vez de falhar calado", {
  limpar_ctl()
  .tr_control$sessoes <- list()
  expect_error(tr_control_state(), "Nenhum editor aberto")
})

test_that("a sessão se registra no ready e sai no fim", {
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p, autosave = FALSE), {
    session$sendCustomMessage <- function(type, message) NULL
    expect_null(.tr_control$sessoes[[session$token]])
    session$setInputs(tr_ready = 1)
    expect_false(is.null(.tr_control$sessoes[[session$token]]))
  })
  expect_length(.tr_control$sessoes, 0L)
})

test_that("op do agente passa pelo aplicar da sessão: rev, log, eco com autor", {
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_ready = 1)
    rev0 <- rv_doc()$rev

    # Sem base_rev: vale sobre a revisão atual (o rebase).
    r <- tr_control_op(list(op = "add_node", id = "a", type = "t/const"))
    expect_true(r$ok)
    expect_equal(r$rev, rev0 + 1L)
    expect_false(is.null(rv_doc()$nodes$a))
    expect_length(session$env$aberto$log, 1L)

    eco <- Filter(function(m) identical(m$type, "op_applied"), msgs)
    expect_equal(eco[[1]]$autor, "agente")
    # Op do agente não foi aplicada pelo front: o documento vai junto sempre.
    expect_true("document" %in% vapply(msgs, function(m) m$type, ""))

    # Com base_rev velho, a proteção estrita continua valendo.
    r <- tr_control_op(list(op = "add_node", id = "b", type = "t/const"), base_rev = rev0)
    expect_false(r$ok)
    expect_equal(r$reason, "stale_rev")

    # Recusa de conteúdo volta estruturada, e o documento não muda.
    r <- tr_control_op(list(op = "add_node", id = "a", type = "t/const"))
    expect_false(r$ok)
    expect_match(r$message, "já existe")
  })
})

test_that("cmd add --from vira UM batch: nó, posição sem sobrepor e ligação", {
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p, autosave = FALSE), {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(tr_ready = 1)
    tr_control_cmd(list(cmd = "add", type = "t/const", id = "a", params = list(value = 2)))
    tr_control_cmd(list(cmd = "add", type = "t/const", id = "b"))
    log0 <- length(session$env$aberto$log)
    r <- tr_control_cmd(list(cmd = "add", type = "t/add", id = "s", from = list("a", "b")))
    expect_true(r$ok)
    expect_equal(r$op$op, "batch")
    expect_length(session$env$aberto$log, log0 + 1L)   # um Ctrl+Z desfaz o gesto inteiro
    para <- vapply(Filter(function(e) e$to$node == "s", rv_doc()$edges), function(e) e$to$port, "")
    expect_setequal(para, c("a", "b"))

    # Filhos da mesma origem e cards com dimensões muito diferentes continuam
    # em colunas distintas: altura desconhecida do preview não causa colisão.
    doc <- rv_doc(); doc$ui$sizes <- list(a = c(320, 900), s = c(700, 1200)); rv_doc(doc)
    tr_control_cmd(list(cmd = "add", type = "t/add", id = "s2", from = list("a")))
    doc <- rv_doc(); doc$ui$sizes$s2 <- c(280, 180); rv_doc(doc)
    tr_control_cmd(list(cmd = "add", type = "t/add", id = "s3", from = list("a")))
    pos <- rv_doc()$ui$positions
    novos <- pos[c("s", "s2", "s3")]
    expect_equal(length(unique(vapply(novos, `[[`, 0, 1))), 3L)
    largura <- vapply(c("s", "s2", "s3"), function(no) {
      rv_doc()$ui$sizes[[no]][[1]] %||% 300
    }, 0)
    x <- vapply(novos, `[[`, 0, 1)
    ord <- order(x)
    expect_true(all(x[ord][-length(ord)] + largura[ord][-length(ord)] < x[ord][-1]))

    r <- tr_control_cmd(list(cmd = "set", node = "a", params = list(value = 5)))
    expect_true(r$ok)
    expect_equal(rv_doc()$nodes$a$params$value, 5)

    expect_error(tr_control_cmd(list(cmd = "link", from = "a", to = "nao_existe")), "nao_existe")
  })
})

test_that("cmd accept aceita a versão nova dos blocos em deriva (#32)", {
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p, autosave = FALSE), {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(tr_ready = 1)
    expect_error(tr_control_cmd(list(cmd = "accept")), "Nenhum bloco")
    tr_control_cmd(list(cmd = "add", type = "t/const", id = "a"))
    tr_control_cmd(list(cmd = "add", type = "t/const", id = "b"))
    # Fluxo salvo com a v0 do bloco: deriva, sem migração.
    doc <- rv_doc(); doc$nodes$a$type_version <- 0L; doc$nodes$b$type_version <- 0L; rv_doc(doc)
    kinds <- function() vapply(tr_control_state()$problems, function(x) x$kind, "")
    expect_equal(sum(kinds() == "version_drift"), 2L)
    r <- tr_control_cmd(list(cmd = "accept", nodes = list("a")))
    expect_true(r$ok)
    expect_equal(r$op$op, "accept_version")
    expect_equal(sum(kinds() == "version_drift"), 1L)
    r <- tr_control_cmd(list(cmd = "accept"))
    expect_true(r$ok)
    expect_false("version_drift" %in% kinds())
  })
})

test_that("op aplicada continua confirmada depois de trabalho lento", {
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p, autosave = FALSE), {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(tr_ready = 1)
    aplicar <- session$env$aplicar
    session$env$aplicar <- function(env) {
      res <- aplicar(env)
      Sys.sleep(0.15) # representa o executor sequencial preso num bloco pesado
      res
    }

    r <- tr_control_op(list(op = "add_node", id = "confirmado", type = "t/const"))
    expect_true(r$ok)
    expect_equal(r$rev, rv_doc()$rev)
    expect_true("confirmado" %in% names(rv_doc()$nodes))
  })
})

test_that("result não executa: nó que não rodou diz que não rodou", {
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p, autosave = FALSE), {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(tr_ready = 1)
    tr_control_op(list(op = "add_node", id = "a", type = "t/const"))
    session$env$estado$a <- NULL
    r <- tr_control_result("a")
    expect_equal(r$status, "idle")
    expect_error(tr_control_result("zz"), "desconhecido")
  })
})

test_that("busca aproximada casa como o '+' do editor", {
  expect_equal(.tr_busca_ponto("anova", "ANOVA em DIC"), 3)
  expect_equal(.tr_busca_ponto("anv", "ANOVA em DIC"), 1)          # letras em sequência
  expect_equal(.tr_busca_ponto("regressao", "Regressão linear"), 3) # sem acento
  expect_equal(.tr_busca_ponto("gress", "Regressão linear"), 2)     # trecho no meio
  expect_equal(.tr_busca_ponto("anova zzz", "ANOVA em DIC"), 0)     # toda palavra casa
  expect_equal(.tr_busca_ponto("  ", "ANOVA"), 0)
})

test_that("catalog --busca devolve só o que casa, melhor primeiro, com total", {
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p, autosave = FALSE), {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(tr_ready = 1)
    tudo <- tr_control_catalog()
    r <- tr_control_catalog(busca = "t/add")
    expect_true(r$ok)
    expect_equal(r$nodes[[1]]$type, "t/add")
    expect_lt(length(r$nodes), length(tudo$nodes))
    expect_equal(r$total, length(r$nodes))
    expect_length(tr_control_catalog(busca = "zzzqqq")$nodes, 0L)
    expect_length(tr_control_catalog(busca = "t", limite = 2L)$nodes, 2L)
    expect_null(tudo$total)
    # "ad" casa "t/add" de verdade; o que só casa por letras soltas sai.
    expect_true(all(vapply(tr_control_catalog(busca = "ad")$nodes,
                           function(n) grepl("ad", tolower(paste(n$type, n$label, n$description))), TRUE)))
  })
})

test_that("nó bloqueado aponta o ancestral que falhou, sem passar dele", {
  e <- function(de, para) list(from = list(node = de, port = "out"), to = list(node = para, port = "x"))
  edges <- list(e("a", "b"), e("b", "c"), e("c", "d"), e("x", "d"))
  status <- c(a = "failed", b = "blocked", c = "blocked", d = "blocked", x = "done")
  expect_equal(.tr_control_causa("d", status, edges), list(list(node = "a", status = "failed")))
  status[["b"]] <- "invalid"
  expect_equal(.tr_control_causa("d", status, edges), list(list(node = "b", status = "invalid")))
  expect_length(.tr_control_causa("x", status, edges), 0L)

  nos <- lapply(names(status), function(id) list(id = id, status = status[[id]]))
  com <- .tr_control_com_causa(nos, edges)
  expect_null(com[[1]]$causa)
  expect_equal(com[[4]]$causa[[1]]$node, "b")
})

test_that("--wait: tocados pela op e tudo abaixo deles", {
  op <- list(op = "batch", ops = list(
    list(op = "add_node", id = "m"),
    list(op = "connect", from_node = "d", to_node = "m")))
  expect_equal(.tr_cli_tocados(op), "m")
  expect_equal(.tr_cli_tocados(list(op = "set_param", node = "d")), "d")
  expect_null(.tr_cli_tocados(list(op = "remove_node", node = "d")))

  st <- list(nodes = lapply(c("d", "m", "c", "g", "z"), function(id) list(id = id)),
             edges = list("d:out -> m:dados", "m:out -> c:modelo", "d:out -> g:dados"))
  ids <- function(x) vapply(x, function(n) n$id, "")
  expect_equal(ids(.tr_cli_abaixo(st, "m")), c("m", "c"))
  expect_equal(ids(.tr_cli_abaixo(st, "d")), c("d", "m", "c", "g"))
  expect_equal(ids(.tr_cli_abaixo(st, NULL)), c("d", "m", "c", "g", "z"))
  expect_equal(ids(.tr_cli_abaixo(list(nodes = st$nodes, edges = list()), "z")), "z")
})

test_that("frame, nota, renomear e organizar pelo canal: ops comuns, um desfazer por gesto (#19, #33)", {
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p, autosave = FALSE), {
    session$sendCustomMessage <- function(type, message) NULL
    session$setInputs(tr_ready = 1)
    tr_control_cmd(list(cmd = "add", type = "t/const", id = "a"))
    tr_control_cmd(list(cmd = "add", type = "t/const", id = "b"))
    tr_control_cmd(list(cmd = "add", type = "t/add", id = "s", from = list("a", "b")))

    r <- tr_control_cmd(list(cmd = "frame", acao = "add", title = "1. Dados", nodes = list("a", "b")))
    expect_true(r$ok)
    fid <- r$id
    expect_equal(rv_doc()$ui$frames[[fid]]$title, "1. Dados")

    # Bloco novo direto na etapa; medida do editor entra no cálculo.
    session$setInputs(tr_medidas = list(a = list(240, 300)))
    r <- tr_control_cmd(list(cmd = "add", type = "t/const", id = "c", frame = fid))
    expect_true(r$ok)
    st <- tr_control_state()
    f <- Filter(function(x) x$id == fid, st$frames)[[1]]
    expect_setequal(unlist(f$nodes), c("a", "b", "c"))
    expect_length(st$nodes[[1]]$position, 2L)

    r <- tr_control_cmd(list(cmd = "note", acao = "add", text = "## Dados", frame = fid))
    expect_true(r$ok)
    nid <- r$id
    expect_equal(tr_control_state()$notes[[1]]$frame, fid)

    log0 <- length(session$env$aberto$log)
    rot0 <- rv_doc()$nodes$s$label
    expect_true(tr_control_cmd(list(cmd = "rename", node = "s", label = "Soma das fontes"))$ok)
    expect_equal(rv_doc()$nodes$s$label, "Soma das fontes")
    tr_control_undo()
    expect_equal(rv_doc()$nodes$s$label, rot0)
    expect_length(session$env$aberto$log, log0)

    antes <- rv_doc()$ui$positions
    r <- tr_control_cmd(list(cmd = "layout"))
    expect_true(r$ok)
    expect_equal(r$op$op, "batch")
    expect_length(session$env$aberto$log, log0 + 1L)
    # De novo: nada a mudar, e nenhum passo vazio na pilha.
    r <- tr_control_cmd(list(cmd = "layout"))
    expect_equal(r$message, "Nada a mudar.")
    expect_length(session$env$aberto$log, log0 + 1L)
    tr_control_undo()
    expect_equal(rv_doc()$ui$positions, antes)

    r <- tr_control_cmd(list(cmd = "move", items = list(list(id = fid, dx = 100, dy = 0))))
    expect_true(r$ok)
    expect_error(tr_control_cmd(list(cmd = "frame", acao = "put", frame = "nada", nodes = list("a"))),
                 "Frame desconhecido")
    expect_true(tr_control_cmd(list(cmd = "note", acao = "rm", note = nid))$ok)
    expect_length(rv_doc()$ui$notes, 0L)
  })
})

# Segunda aba do mesmo editor. `testServer()` não aninha; a sessão falsa roda
# o servidor à mão, no próprio domínio, e fecha com `$close()`.
aba_nova <- function(server) {
  s <- shiny::MockShinySession$new()
  s$sendCustomMessage <- function(type, message) NULL
  shiny::isolate(shiny::withReactiveDomain(s, server(input = s$input, output = s$output, session = s)))
  s$setInputs(tr_ready = 1)
  s
}

test_that("duas abas no mesmo fluxo: a que fecha não leva as edições do agente", {
  # O caso real: um print com Chrome headless abriu uma segunda aba, o canal
  # passou a falar com ela, e quando ela caiu a edição seguinte foi para a aba
  # antiga, com o documento antigo. O autosave dela apagou 7 frames do disco.
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_ready = 1)
    velha <- session$token

    nova <- aba_nova(tr_server(p))
    expect_identical(.tr_control_sessao()$session$token, nova$token)
    expect_true(tr_control_cmd(list(cmd = "frame", acao = "add", title = "1. Dados"))$ok)
    expect_true(tr_control_op(list(op = "add_node", id = "a", type = "t/const"))$ok)
    nova$close()

    # A nova fechou: o canal volta para a velha, que já tem o documento atual.
    expect_identical(.tr_control_sessao()$session$token, velha)
    expect_length(rv_doc()$ui$frames, 1L)
    docs <- Filter(function(m) identical(m$type, "document"), msgs)
    expect_true("a" %in% names(docs[[length(docs)]]$doc$nodes))

    expect_true(tr_control_op(list(op = "add_node", id = "b", type = "t/const"))$ok)
    disco <- tr_project_flow(p, "main")
    expect_setequal(names(disco$nodes), c("a", "b"))
    expect_length(disco$ui$frames, 1L)

    # Um fluxo, uma história: o Ctrl+Z da aba velha desfaz a op da nova.
    tr_control_undo(); tr_control_undo()
    expect_length(rv_doc()$nodes, 0L)
    expect_length(rv_doc()$ui$frames, 1L)
  })
})

test_that("edição numa aba chega à outra aberta, e o autosave dela não a desfaz", {
  limpar_ctl()
  p <- projeto_ctl()
  shiny::testServer(tr_server(p), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_ready = 1)
    nova <- aba_nova(tr_server(p))
    nova$setInputs(tr_op = list(seq = 1, base_rev = rv_doc()$rev,
                                op = list(op = "add_node", id = "a", type = "t/const")))
    expect_false(is.null(rv_doc()$nodes$a))
    expect_true(any(vapply(msgs, function(m) identical(m$type, "document") &&
                                    "a" %in% names(m$doc$nodes), TRUE)))
    session$setInputs(tr_op = list(seq = 2, base_rev = rv_doc()$rev,
                                   op = list(op = "add_node", id = "b", type = "t/const")))
    expect_setequal(names(tr_project_flow(p, "main")$nodes), c("a", "b"))
    nova$close()
  })
  # A última aba saiu: o documento do processo vai embora, e a próxima lê o disco.
  expect_length(ls(.tr_abertos), 0L)
})
