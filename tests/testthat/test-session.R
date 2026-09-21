test_that("base_rev defasado é recusado sem corromper o documento", {
  reg <- test_registry()
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/const", id = "a"), reg)
  expect_equal(doc$rev, 1L)

  res <- tr_submit(doc, list(seq = 1, base_rev = 0L,
                             op = list(op = "add_node", type = "t/const", id = "b")), reg)
  expect_false(res$ok)
  expect_equal(res$reason, "stale_rev")
  expect_identical(res$doc, doc)
})

test_that("op válida avança a revisão e ecoa seq", {
  reg <- test_registry()
  doc <- tr_doc()
  res <- tr_submit(doc, list(seq = 7, base_rev = 0L,
                             op = list(op = "add_node", type = "t/const", id = "a")), reg)
  expect_true(res$ok)
  expect_equal(res$seq, 7)
  expect_equal(res$rev, 1L)
  expect_true(res$semantic)
})

test_that("op inválida é rejeitada como valor, não como exceção", {
  reg <- test_registry()
  doc <- tr_doc()
  res <- tr_submit(doc, list(seq = 1, base_rev = 0L,
                             op = list(op = "set_param", node = "sumiu", name = "k", value = 1)), reg)
  expect_false(res$ok)
  expect_equal(res$reason, "rejected")
  expect_equal(res$class, "tr_error_unknown_node")
  expect_identical(res$doc, doc)
})

test_that("mover não conta como mudança semântica", {
  reg <- test_registry()
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/const", id = "a"), reg)
  res <- tr_submit(doc, list(seq = 2, base_rev = 1L,
                             op = list(op = "move", node = "a", x = 5, y = 6)), reg)
  expect_true(res$ok)
  expect_false(res$semantic)
})

test_that("replay do log reconstrói o documento (base do undo)", {
  reg <- test_registry()
  ops <- list(
    list(op = "add_node", type = "t/const", id = "a", seed = 1L),
    list(op = "add_node", type = "t/add", id = "s", seed = 2L),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "s", to_port = "a"),
    list(op = "set_param", node = "s", name = "k", value = 5)
  )
  full <- tr_replay(ops, reg)
  undone <- tr_replay(ops[1:3], reg)
  expect_equal(full$nodes$s$params$k, 5)
  expect_null(undone$nodes$s$params$k)
  expect_equal(tr_doc_json(tr_replay(ops, reg)), tr_doc_json(full))
})

# Undo é replay do log da SESSÃO, e esse log só cobre a sessão: começa no
# documento que o servidor mandou quando o front montou, não no vazio. Base
# construída por ops pra simular um fluxo aberto do disco com história própria.
undo_base <- function(reg) {
  doc <- add(tr_doc(), reg, "t/const", id = "a", seed = 1L)
  add(doc, reg, "t/add", id = "s", seed = 2L)
}

test_that("desfazer a primeira op da sessão preserva o fluxo aberto", {
  reg <- test_registry()
  base <- undo_base(reg)
  depois <- tr_doc_apply(base, list(op = "move", node = "a", x = 50, y = 60), reg)

  doc <- .tr_undo_doc(base, list(), current_rev = depois$rev, registry = reg)
  expect_setequal(names(doc$nodes), c("a", "s"))
  expect_equal(doc$nodes, base$nodes)
  expect_equal(doc$edges, base$edges)
})

test_that("desfazer mantém as ops anteriores do log sobre a base", {
  reg <- test_registry()
  base <- undo_base(reg)
  ops <- list(
    list(op = "add_node", type = "t/const", id = "b", seed = 3L),
    list(op = "set_param", node = "s", name = "k", value = 5)
  )
  depois <- tr_replay(ops, reg, doc = base)

  doc <- .tr_undo_doc(base, ops[1], current_rev = depois$rev, registry = reg)
  esperado <- tr_replay(ops[1], reg, doc = base)
  expect_setequal(names(doc$nodes), c("a", "s", "b"))
  expect_null(doc$nodes$s$params$k)
  expect_equal(doc$nodes, esperado$nodes)
})

test_that("a revisão do undo é monotônica, nunca volta", {
  reg <- test_registry()
  base <- undo_base(reg)
  doc <- .tr_undo_doc(base, list(), current_rev = 323L, registry = reg)
  expect_identical(doc$rev, 324L)
  expect_gt(doc$rev, 323L)
})

# A fiação de verdade, e não só `.tr_undo_doc()`: o log mora no servidor e cresce
# com a op NORMALIZADA de cada `tr_op` aplicado. Quando o log era do cliente,
# um Ctrl+Z com op em voo desfazia o passo errado — este teste trava que o
# `tr_undo` não depende de nada que o cliente mande.
test_that("tr_undo desfaz a última op aplicada, pelo log do servidor", {
  reg <- store_registry()
  root <- withr::local_tempdir("proj")
  dir.create(file.path(root, "flows"), recursive = TRUE)
  s <- tr_store(file.path(root, ".trama", "store"), project_root = root)
  # Mesmo projeto montado à mão de test-runs.R: a coleção de teste não é pacote.
  project <- structure(list(root = root, registry = reg, store = s,
                            flows_dir = file.path(root, "flows")), class = "tr_project")
  doc0 <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "a", params = list(v = 1)),
    list(op = "add_node", type = "t/inc", id = "b"),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "b", to_port = "x"),
    list(op = "move", node = "a", x = 1, y = 2)))
  tr_doc_write(doc0, file.path(root, "flows", "main.json"))

  shiny::testServer(tr_server(project, autosave = FALSE), {
    # O mock do Shiny não entrega mensagem nenhuma; aqui elas ficam guardadas
    # pra o teste ver o que o front receberia.
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    tipos <- function() vapply(msgs, function(m) m$type, "")
    # `testServer` avalia este bloco numa CÓPIA do ambiente do servidor: o que
    # é reativo (`rv_doc`) é referência e acompanha, mas `log` é reatribuído
    # com `<<-` no ambiente original, e só `session$env` enxerga isso.
    log_len <- function() length(session$env$log)

    session$setInputs(tr_ready = 1)
    rev0 <- rv_doc()$rev
    session$setInputs(tr_op = list(seq = 1, base_rev = rev0,
                                   op = list(op = "move", node = "b", x = 5, y = 6)))
    expect_equal(rv_doc()$ui$positions$b, c(5, 6), ignore_attr = TRUE)
    expect_equal(log_len(), 1L)
    # Duas ops no log: um undo tem que tirar SÓ a segunda. Com uma só, "desfazer
    # a última" e "desfazer tudo" dariam o mesmo documento.
    session$setInputs(tr_op = list(seq = 2, base_rev = rv_doc()$rev,
                                   op = list(op = "move", node = "b", x = 7, y = 8)))
    expect_equal(log_len(), 2L)

    msgs <- list()
    session$setInputs(tr_undo = 2)
    expect_equal(rv_doc()$ui$positions$b, c(5, 6), ignore_attr = TRUE)
    expect_equal(log_len(), 1L)

    session$setInputs(tr_undo = 3)
    doc <- rv_doc()
    expect_setequal(names(doc$nodes), c("a", "b"))
    expect_length(doc$edges, 1L)
    expect_equal(doc$ui$positions, doc0$ui$positions)
    expect_gt(doc$rev, rev0 + 1L)
    expect_equal(log_len(), 0L)
    expect_true("document" %in% tipos())

    # Log vazio: não há o que desfazer, e nada muda — nem revisão, nem mensagem.
    msgs <- list()
    rev1 <- doc$rev
    session$setInputs(tr_undo = 4)
    expect_identical(rv_doc()$rev, rev1)
    expect_equal(log_len(), 0L)
    expect_false("document" %in% tipos())
  })
})

test_that("o editor recebe código R ou Quarto para baixar", {
  reg <- store_registry()
  root <- withr::local_tempdir("proj")
  dir.create(file.path(root, "flows"), recursive = TRUE)
  s <- tr_store(file.path(root, ".trama", "store"), project_root = root)
  project <- structure(list(root = root, registry = reg, store = s,
                            flows_dir = file.path(root, "flows")), class = "tr_project")
  doc <- build(reg, list(list(op = "add_node", type = "t/const", id = "origem",
                              params = list(v = 7))))
  tr_doc_write(doc, file.path(root, "flows", "main.json"))

  shiny::testServer(tr_server(project, autosave = FALSE), {
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- message
    session$setInputs(tr_ready = 1)

    msgs <- list()
    session$setInputs(tr_export_code = list(format = "r", seq = 1))
    export <- Filter(function(m) identical(m$type, "export_code"), msgs)
    expect_length(export, 1L)
    expect_identical(export[[1]]$format, "r")
    expect_no_match(export[[1]]$code, "tr_flow")

    msgs <- list()
    session$setInputs(tr_export_code = list(format = "quarto", seq = 2))
    export <- Filter(function(m) identical(m$type, "export_code"), msgs)
    expect_length(export, 1L)
    expect_identical(export[[1]]$format, "quarto")
    expect_match(export[[1]]$code, "```\\{r\\}")
  })
})
