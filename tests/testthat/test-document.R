test_that("add_node materializa seed e id, e registra a coleção usada", {
  x <- mk()
  doc <- add(x$doc, x$reg, "t/const", id = "a")
  expect_equal(doc$rev, 1L)
  expect_true(is.integer(doc$nodes$a$seed))
  expect_equal(doc$nodes$a$type_version, 1L)
  expect_equal(doc$collections$t, "1.0.0")

  # id gerado é único e ordenável por criação
  d2 <- add(doc, x$reg, "t/const")
  ids <- setdiff(names(d2$nodes), "a")
  expect_match(ids, "^[0-9A-Z]{16}$")
})

test_that("renomear e mover NÃO são semânticos; param e seed são", {
  expect_false(tr_op_semantic(list(op = "rename")))
  expect_false(tr_op_semantic(list(op = "move")))
  expect_true(tr_op_semantic(list(op = "set_param")))
  expect_true(tr_op_semantic(list(op = "set_seed")))
  expect_true(tr_op_semantic(list(op = "connect")))
})

test_that("resize é cosmético e guarda o tamanho no ui", {
  expect_false(tr_op_semantic(list(op = "resize")))
  x <- mk(); d0 <- add(x$doc, x$reg, "t/const", id = "a")
  doc <- tr_doc_apply(d0, list(op = "resize", node = "a", w = 480, h = 300), x$reg)
  expect_equal(doc$ui$sizes[["a"]], c(480, 300))
})

test_that("resize em nó inexistente aborta", {
  x <- mk()
  expect_error(tr_doc_apply(x$doc, list(op = "resize", node = "zzz", w = 1, h = 1), x$reg),
               class = "tr_error_unknown_node")
})

test_that("set_view é cosmético e guarda o id da vista", {
  expect_false(tr_op_semantic(list(op = "set_view")))
  x <- mk(); d0 <- add(x$doc, x$reg, "t/const", id = "a")
  doc <- tr_doc_apply(d0, list(op = "set_view", node = "a", view = "resumo"), x$reg)
  expect_equal(doc$ui$views[["a"]], "resumo")
})

test_that("set_view em nó inexistente aborta", {
  x <- mk()
  expect_error(tr_doc_apply(x$doc, list(op = "set_view", node = "zzz", view = "resumo"), x$reg),
               class = "tr_error_unknown_node")
})

test_that("renomear não altera identidade nem seed", {
  x <- mk(); doc <- add(x$doc, x$reg, "t/const", id = "a")
  seed <- doc$nodes$a$seed
  doc <- tr_doc_apply(doc, list(op = "rename", node = "a", label = "Outro"), x$reg)
  expect_equal(names(doc$nodes), "a")
  expect_equal(doc$nodes$a$seed, seed)
})

test_that("connect valida porta, tipo e ciclo", {
  x <- mk()
  doc <- add(x$doc, x$reg, "t/const", id = "a")
  doc <- add(doc, x$reg, "t/add", id = "s")
  doc <- tr_doc_apply(doc, list(op = "connect", from_node = "a", from_port = "out",
                                to_node = "s", to_port = "a"), x$reg)
  expect_length(doc$edges, 1)

  expect_error(tr_doc_apply(doc, list(op = "connect", from_node = "a", from_port = "nope",
                                      to_node = "s", to_port = "a"), x$reg),
               class = "tr_error_unknown_port")
  # Reconectar a MESMA aresta numa porta comum é idempotente, não erro: o
  # gesto de arrastar o cabo de novo pro mesmo lugar não deve reclamar.
  again <- tr_doc_apply(doc, list(op = "connect", from_node = "a", from_port = "out",
                                  to_node = "s", to_port = "a"), x$reg)
  expect_length(again$edges, 1)
})

test_that("aresta usa adaptador quando os tipos diferem mas são compatíveis", {
  x <- mk()
  doc <- add(x$doc, x$reg, "t/const", id = "a")
  doc <- add(doc, x$reg, "t/show", id = "v")   # espera t/txt, recebe t/num
  doc <- tr_doc_apply(doc, list(op = "connect", from_node = "a", from_port = "out",
                                to_node = "v", to_port = "x"), x$reg)
  expect_length(doc$edges, 1)
})

test_that("ciclo é recusado na edição, não na execução", {
  x <- mk()
  doc <- add(x$doc, x$reg, "t/add", id = "p")
  doc <- add(doc, x$reg, "t/add", id = "q")
  doc <- tr_doc_apply(doc, list(op = "connect", from_node = "p", from_port = "out",
                                to_node = "q", to_port = "a"), x$reg)
  expect_error(
    tr_doc_apply(doc, list(op = "connect", from_node = "q", from_port = "out",
                           to_node = "p", to_port = "a"), x$reg),
    class = "tr_error_cycle"
  )
})

test_that("porta não-variádica substitui a aresta anterior", {
  x <- mk()
  doc <- add(x$doc, x$reg, "t/const", id = "a")
  doc <- add(doc, x$reg, "t/const", id = "b")
  doc <- add(doc, x$reg, "t/add", id = "s")
  doc <- tr_doc_apply(doc, list(op = "connect", from_node = "a", from_port = "out",
                                to_node = "s", to_port = "a"), x$reg)
  doc <- tr_doc_apply(doc, list(op = "connect", from_node = "b", from_port = "out",
                                to_node = "s", to_port = "a"), x$reg)
  expect_length(doc$edges, 1)
  expect_equal(doc$edges[[1]]$from$node, "b")
})

test_that("remover nó leva junto as arestas dele", {
  x <- mk()
  doc <- add(x$doc, x$reg, "t/const", id = "a")
  doc <- add(doc, x$reg, "t/add", id = "s")
  doc <- tr_doc_apply(doc, list(op = "connect", from_node = "a", from_port = "out",
                                to_node = "s", to_port = "a"), x$reg)
  doc <- tr_doc_apply(doc, list(op = "remove_node", node = "a"), x$reg)
  expect_length(doc$edges, 0)
  expect_null(doc$nodes$a)
})

test_that("remover nó leva junto o estado cosmético dele", {
  x <- mk(); d0 <- add(x$doc, x$reg, "t/const", id = "a")
  doc <- tr_doc_apply(d0, list(op = "resize", node = "a", w = 480, h = 300), x$reg)
  doc <- tr_doc_apply(doc, list(op = "set_view", node = "a", view = "resumo"), x$reg)
  doc <- tr_doc_apply(doc, list(op = "set_fold", node = "a", preview = FALSE), x$reg)
  doc <- tr_doc_apply(doc, list(op = "remove_node", node = "a"), x$reg)
  expect_null(doc$ui$positions[["a"]])
  expect_null(doc$ui$sizes[["a"]])
  expect_null(doc$ui$views[["a"]])
  expect_null(doc$ui$folds[["a"]])
})

test_that("param desconhecido é recusado", {
  x <- mk(); doc <- add(x$doc, x$reg, "t/const", id = "a")
  expect_error(tr_doc_apply(doc, list(op = "set_param", node = "a", name = "nope", value = 1), x$reg),
               class = "tr_error_unknown_param")
})

test_that("terminais são os nós sem aresta de saída", {
  x <- mk()
  doc <- add(x$doc, x$reg, "t/const", id = "a")
  doc <- add(doc, x$reg, "t/add", id = "s")
  doc <- tr_doc_apply(doc, list(op = "connect", from_node = "a", from_port = "out",
                                to_node = "s", to_port = "a"), x$reg)
  expect_equal(tr_doc_terminals(doc), "s")
})

test_that("batch aplica tudo numa revisão só", {
  m <- mk()
  doc <- tr_doc_apply(m$doc, list(op = "batch", ops = list(
    list(op = "add_node", type = "t/const", id = "a"),
    list(op = "move", node = "a", x = 10, y = 20),
    list(op = "rename", node = "a", label = "Outro"))), m$reg)
  expect_equal(doc$rev, 1L)
  expect_equal(doc$ui$positions[["a"]], c(10, 20))
  expect_equal(doc$nodes$a$label, "Outro")
})

test_that("batch ecoa as ops normalizadas, e o replay reconstrói os mesmos ids", {
  m <- mk()
  res <- tr_submit(m$doc, list(seq = 1, base_rev = 0, op = list(op = "batch", ops = list(
    list(op = "add_node", type = "t/const"),
    list(op = "add_node", type = "t/const")))), m$reg)
  expect_true(res$ok)
  expect_false(is.null(res$op$ops[[1]]$id))
  expect_false(is.null(res$op$ops[[1]]$seed))
  back <- tr_replay(list(res$op), m$reg)
  expect_identical(sort(names(back$nodes)), sort(names(res$doc$nodes)))
})

test_that("batch é atômico: uma op ruim deixa o documento intacto", {
  m <- mk(); d0 <- add(m$doc, m$reg, "t/const", id = "a")
  res <- tr_submit(d0, list(seq = 1, base_rev = d0$rev, op = list(op = "batch", ops = list(
    list(op = "move", node = "a", x = 1, y = 1),
    list(op = "move", node = "a", x = 2, y = 2),
    list(op = "move", node = "zzz", x = 3, y = 3)))), m$reg)
  expect_false(res$ok)
  expect_identical(res$doc, d0)
  expect_match(res$message, "op 3 de 3 (move)", fixed = TRUE)
  expect_equal(res$class, "tr_error_unknown_node")
})

test_that("batch recusa lista vazia, ausente e batch aninhado", {
  m <- mk(); d0 <- add(m$doc, m$reg, "t/const", id = "a")
  expect_error(tr_doc_apply(d0, list(op = "batch", ops = list()), m$reg), class = "tr_error_bad_op")
  expect_error(tr_doc_apply(d0, list(op = "batch"), m$reg), class = "tr_error_bad_op")
  expect_error(tr_doc_apply(d0, list(op = "batch", ops = list(
    list(op = "batch", ops = list(list(op = "rename", node = "a", label = "x"))))), m$reg),
    class = "tr_error_bad_op")
})

test_that("batch diz o que está errado: ops não-lista, item não-objeto, aninhado", {
  m <- mk(); d0 <- add(m$doc, m$reg, "t/const", id = "a")
  ap <- function(ops) tr_doc_apply(d0, list(op = "batch", ops = ops), m$reg)
  expect_error(ap("move"), "'ops' deve ser uma lista não vazia.", fixed = TRUE,
               class = "tr_error_bad_op")
  expect_error(ap(list()), "'ops' deve ser uma lista não vazia.", fixed = TRUE,
               class = "tr_error_bad_op")
  expect_error(ap(list(1, 2)), "op 1 de 2 não é um objeto.", fixed = TRUE,
               class = "tr_error_bad_op")
  expect_error(ap(list(list(op = "rename", node = "a", label = "x"), list(op = "batch", ops = list()))),
               "op 2 de 2: batch não pode conter batch.", fixed = TRUE,
               class = "tr_error_bad_op")
})

test_that("campo 'op' que não é um texto é op malformada, não despacho por posição", {
  m <- mk(); d0 <- add(m$doc, m$reg, "t/const", id = "a")
  for (bad in list(5, 100, list("move"), c("move", "rename"), NA_character_)) {
    expect_error(tr_doc_apply(d0, list(op = bad, node = "a", label = "x"), m$reg),
                 "Op malformada: campo 'op' ausente ou inválido.", fixed = TRUE,
                 class = "tr_error_bad_op")
  }
  expect_error(tr_doc_apply(d0, list(node = "a"), m$reg), class = "tr_error_bad_op")
  expect_error(tr_doc_apply(d0, list(op = "voar"), m$reg), class = "tr_error_unknown_op")
  # Dentro do batch, o erro da op interna não pode quebrar o próprio sprintf
  # que o reembala.
  expect_error(tr_doc_apply(d0, list(op = "batch", ops = list(list(op = list("move")))), m$reg),
               "op 1 de 1 (?)", fixed = TRUE, class = "tr_error_bad_op")
})

test_that("batch é semântico se QUALQUER op dentro dele for", {
  expect_false(tr_op_semantic(list(op = "batch", ops = list(list(op = "move"), list(op = "resize")))))
  expect_true(tr_op_semantic(list(op = "batch", ops = list(list(op = "move"), list(op = "remove_node")))))
})

test_that("ops de frame e set_fold são cosméticas", {
  for (o in c("add_frame", "update_frame", "remove_frame", "reorder_frames", "set_fold")) {
    expect_false(tr_op_semantic(list(op = o)), info = o)
  }
})

test_that("add_frame materializa id, ordem e padrões", {
  m <- mk()
  d1 <- tr_doc_apply(m$doc, list(op = "add_frame", x = 0, y = 0, w = 1600, h = 900), m$reg)
  id1 <- names(d1$ui$frames)
  expect_match(id1, "^[0-9A-Z]{16}$")
  f <- d1$ui$frames[[id1]]
  expect_equal(f$order, 1L)
  expect_equal(f$aspect, "16:9")
  expect_equal(f$color, "azul")
  expect_equal(f$title, "Frame 1")

  d2 <- tr_doc_apply(d1, list(op = "add_frame", x = 0, y = 0, w = 10, h = 10,
                              title = "Dois", aspect = "livre", color = "rosa"), m$reg)
  f2 <- d2$ui$frames[[setdiff(names(d2$ui$frames), id1)]]
  expect_equal(f2$order, 2L)
  expect_equal(f2$title, "Dois")
  expect_equal(f2$aspect, "livre")
  expect_equal(f2$color, "rosa")
})

test_that("add_frame ecoa id e ordem, e o replay reconstrói o mesmo frame", {
  m <- mk()
  res <- tr_submit(m$doc, list(seq = 1, base_rev = 0,
                               op = list(op = "add_frame", x = 0, y = 0, w = 160, h = 90)), m$reg)
  expect_false(is.null(res$op$id))
  expect_equal(res$op$order, 1L)
  back <- tr_replay(list(res$op), m$reg)
  expect_identical(back$ui$frames, res$doc$ui$frames)
})

test_that("prancheta: batch de add_frame dá a ordem das ops e uma revisão", {
  m <- mk()
  ops <- lapply(1:10, function(i) list(op = "add_frame", x = i * 100, y = 0, w = 90, h = 90,
                                         title = sprintf("F%d", i)))
  d <- tr_doc_apply(m$doc, list(op = "batch", ops = ops), m$reg)
  expect_equal(d$rev, 1L)
  fs <- d$ui$frames
  expect_length(fs, 10L)
  ord <- vapply(fs, function(f) f$order, integer(1))
  tit <- vapply(fs, function(f) f$title, "")
  expect_equal(unname(tit[order(ord)]), sprintf("F%d", 1:10))
})

test_that("add_frame valida retângulo e proporção", {
  m <- mk()
  bad <- function(...) {
    expect_error(tr_doc_apply(m$doc, list(op = "add_frame", ...), m$reg), class = "tr_error_bad_op")
  }
  bad(x = 0, y = 0, w = 0, h = 10)
  bad(x = 0, y = 0, w = 10, h = -1)
  bad(x = "a", y = 0, w = 10, h = 10)
  bad(x = 0, y = 0, w = 10, h = 10, aspect = "3:2")
  bad(x = 0, y = 0, w = 10, h = 10, title = 5)
  bad(x = 0, y = 0, w = 10)
  # A ordem é inteira >= 1: negativa não entra, e grande demais não pode
  # virar NA com warning no as.integer().
  bad(x = 0, y = 0, w = 10, h = 10, order = -3)
  bad(x = 0, y = 0, w = 10, h = 10, order = 0)
  bad(x = 0, y = 0, w = 10, h = 10, order = 1e10)
  d <- tr_doc_apply(m$doc, list(op = "add_frame", id = "f", x = 0, y = 0, w = 10, h = 10,
                                order = 7), m$reg)
  expect_equal(d$ui$frames$f$order, 7L)
})

test_that("update_frame muda só o que veio", {
  m <- mk()
  d <- tr_doc_apply(m$doc, list(op = "add_frame", id = "f", x = 0, y = 0, w = 160, h = 90), m$reg)
  d <- tr_doc_apply(d, list(op = "update_frame", frame = "f", x = 5, title = "Novo"), m$reg)
  f <- d$ui$frames$f
  expect_equal(f$x, 5)
  expect_equal(f$y, 0)
  expect_equal(f$w, 160)
  expect_equal(f$title, "Novo")
  expect_equal(f$order, 1L)
  expect_error(tr_doc_apply(d, list(op = "update_frame", frame = "f"), m$reg),
               class = "tr_error_bad_op")
  expect_error(tr_doc_apply(d, list(op = "update_frame", frame = "f", aspect = "3:2"), m$reg),
               class = "tr_error_bad_op")
  expect_error(tr_doc_apply(d, list(op = "update_frame", frame = "zzz", x = 1), m$reg),
               class = "tr_error_unknown_frame")
})

test_that("remove_frame tira o frame e deixa os cards", {
  m <- mk(); d <- add(m$doc, m$reg, "t/const", id = "a", position = list(10, 10))
  d <- tr_doc_apply(d, list(op = "add_frame", id = "f", x = 0, y = 0, w = 400, h = 300), m$reg)
  d <- tr_doc_apply(d, list(op = "remove_frame", frame = "f"), m$reg)
  expect_null(d$ui$frames$f)
  expect_false(is.null(d$nodes$a))
  expect_equal(d$ui$positions$a, c(10, 10))
  expect_error(tr_doc_apply(d, list(op = "remove_frame", frame = "f"), m$reg),
               class = "tr_error_unknown_frame")
})

test_that("remove_node não toca em frames", {
  m <- mk(); d <- add(m$doc, m$reg, "t/const", id = "a", position = list(10, 10))
  d <- tr_doc_apply(d, list(op = "add_frame", id = "f", x = 0, y = 0, w = 400, h = 300), m$reg)
  d <- tr_doc_apply(d, list(op = "remove_node", node = "a"), m$reg)
  expect_equal(d$ui$frames$f$w, 400)
})

test_that("reorder_frames reescreve a ordem e exige a lista completa", {
  m <- mk(); d <- m$doc
  for (id in c("f1", "f2", "f3")) {
    d <- tr_doc_apply(d, list(op = "add_frame", id = id, x = 0, y = 0, w = 10, h = 10), m$reg)
  }
  d <- tr_doc_apply(d, list(op = "reorder_frames", frames = list("f3", "f1", "f2")), m$reg)
  expect_equal(vapply(d$ui$frames[c("f1", "f2", "f3")], function(f) f$order, integer(1)),
               c(f1 = 2L, f2 = 3L, f3 = 1L))
  for (bad in list(list("f1", "f2"), list("f1", "f2", "f2"), list("f1", "f2", "zz"))) {
    expect_error(tr_doc_apply(d, list(op = "reorder_frames", frames = bad), m$reg),
                 class = "tr_error_bad_op")
  }
})

test_that("set_fold guarda só o que está recolhido", {
  m <- mk(); d <- add(m$doc, m$reg, "t/const", id = "a")
  d <- tr_doc_apply(d, list(op = "set_fold", node = "a", preview = FALSE), m$reg)
  expect_equal(d$ui$folds$a, list(preview = FALSE))
  d <- tr_doc_apply(d, list(op = "set_fold", node = "a", params = FALSE), m$reg)
  expect_equal(d$ui$folds$a, list(preview = FALSE, params = FALSE))
  d <- tr_doc_apply(d, list(op = "set_fold", node = "a", preview = TRUE, params = TRUE), m$reg)
  expect_null(d$ui$folds$a)
})

test_that("set_fold valida o que recebe", {
  m <- mk(); d <- add(m$doc, m$reg, "t/const", id = "a")
  expect_error(tr_doc_apply(d, list(op = "set_fold", node = "a"), m$reg),
               class = "tr_error_bad_op")
  expect_error(tr_doc_apply(d, list(op = "set_fold", node = "a", preview = "não"), m$reg),
               class = "tr_error_bad_op")
  expect_error(tr_doc_apply(d, list(op = "set_fold", node = "zzz", preview = FALSE), m$reg),
               class = "tr_error_unknown_node")
})

test_that("o documento volta ao front quando a estrutura muda, inclusive dentro de batch", {
  expect_true(.tr_op_echoes_doc(list(op = "remove_node")))
  expect_true(.tr_op_echoes_doc(list(op = "add_frame")))
  expect_true(.tr_op_echoes_doc(list(op = "reorder_frames")))
  expect_false(.tr_op_echoes_doc(list(op = "update_frame")))
  expect_false(.tr_op_echoes_doc(list(op = "set_fold")))
  expect_false(.tr_op_echoes_doc(list(op = "batch", ops = list(list(op = "move"), list(op = "update_frame")))))
  expect_true(.tr_op_echoes_doc(list(op = "batch", ops = list(list(op = "move"), list(op = "remove_node")))))
})
