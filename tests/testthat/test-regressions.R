# Cada teste aqui prova um defeito concreto achado na revisão de E1/E2.

test_that("replay reconstrói o documento quando id e seed são gerados", {
  reg <- test_registry()
  # O cliente NÃO manda id nem seed — o servidor materializa. O log que o
  # cliente guarda pro undo é o ECO, e o eco tem que trazer o que foi criado;
  # senão o replay produz ids e seeds diferentes e toda a identidade estável
  # evapora no primeiro undo, invalidando o cache inteiro a jusante.
  doc <- tr_doc(); log <- list()
  for (op in list(list(op = "add_node", type = "t/const"),
                  list(op = "add_node", type = "t/add"))) {
    res <- tr_submit(doc, list(seq = length(log) + 1L, base_rev = doc$rev, op = op), reg)
    expect_true(res$ok)
    doc <- res$doc; log[[length(log) + 1]] <- res$op
  }
  expect_equal(tr_doc_json(tr_replay(log, reg)), tr_doc_json(doc))
})

test_that("base_rev ausente ou inválido é recusado, não isento", {
  reg <- test_registry()
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/const", id = "a"), reg)
  for (bad in list(NULL, "x", NA, list())) {
    res <- tr_submit(doc, list(seq = 1, base_rev = bad,
                               op = list(op = "add_node", type = "t/const", id = "b")), reg)
    expect_false(res$ok)
    expect_equal(res$reason, "malformed")
  }
})

test_that("param vetorial sobrevive ao round-trip com o mesmo typeof", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/num")),
                       nodes = list(tr_node("t/sel", fn = function(cols) cols,
                                            description = "Devolve as colunas escolhidas no param.",
                                            outputs = list(out = "t/num"),
                                            params = list(cols = tr_param("cols", c("x", "y")))))),
         registry = reg)
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/sel", id = "a",
                                     params = list(cols = c("x", "y"))), reg)
  back <- tr_doc_parse(tr_doc_json(doc))
  expect_equal(back$nodes$a$params$cols, c("x", "y"))
  expect_equal(typeof(back$nodes$a$params$cols), "character")
})

test_that("porta variádica aceita a mesma origem duas vezes, com index distinto", {
  reg <- tr_registry()
  tr_use(tr_collection(
    id = "t", types = list(tr_type("t/num")),
    nodes = list(
      tr_node("t/c", fn = function() 1, description = "Devolve a constante um.",
              outputs = list(out = "t/num")),
      tr_node("t/merge", fn = function(xs) xs, outputs = list(out = "t/num"),
              description = "Junta numa lista todas as entradas ligadas.",
              inputs = list(xs = tr_port("t/num", multiple = TRUE))))), registry = reg)
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/c", id = "a"), reg)
  doc <- tr_doc_apply(doc, list(op = "add_node", type = "t/merge", id = "m"), reg)
  cn <- function(d) tr_doc_apply(d, list(op = "connect", from_node = "a", from_port = "out",
                                         to_node = "m", to_port = "xs"), reg)
  doc <- cn(cn(doc))
  expect_length(doc$edges, 2)
  expect_equal(vapply(doc$edges, function(e) e$index, integer(1)), c(1L, 2L))

  # E dá pra desconectar UMA delas, pelo index.
  doc <- tr_doc_apply(doc, list(op = "disconnect", from_node = "a", from_port = "out",
                                to_node = "m", to_port = "xs", index = 1L), reg)
  expect_length(doc$edges, 1)
  expect_equal(doc$edges[[1]]$index, 2L)
})

test_that("tr_use falha sem sujar o registro, e o retry funciona", {
  reg <- tr_registry()
  quebrada <- tr_collection(id = "t", types = list(tr_type("t/num")),
                            nodes = list(tr_node("t/n", fn = function(x) x,
                                                 description = "Repassa a entrada — de um tipo que não existe.",
                                                 inputs = list(x = "t/sumiu"))))
  expect_error(tr_use(quebrada, registry = reg), class = "tr_error_unknown_type")
  expect_length(reg$types, 0)
  expect_length(reg$nodes, 0)
  # Corrigida, carrega — sem precisar reiniciar a sessão.
  expect_silent(tr_use(test_collection(), registry = reg))
  expect_length(reg$nodes, 3)
})

test_that("ciclo é checado em tempo usável e sem estourar a pilha", {
  reg <- test_registry()
  doc <- tr_doc()
  for (i in 1:300) {
    doc <- tr_doc_apply(doc, list(op = "add_node", type = "t/add", id = paste0("n", i)), reg)
  }
  t <- system.time({
    for (i in 1:299) {
      doc <- tr_doc_apply(doc, list(op = "connect", from_node = paste0("n", i), from_port = "out",
                                    to_node = paste0("n", i + 1L), to_port = "a"), reg)
    }
  })[["elapsed"]]
  expect_lt(t, 5)
  expect_error(
    tr_doc_apply(doc, list(op = "connect", from_node = "n300", from_port = "out",
                           to_node = "n1", to_port = "a"), reg),
    class = "tr_error_cycle"
  )
})

test_that("op malformada devolve classe acionável, nunca simpleError", {
  reg <- test_registry(); doc <- tr_doc()
  for (op in list(list(), list(op = "move"), list(op = "connect", from_node = "a"))) {
    res <- tr_submit(doc, list(seq = 1, base_rev = 0L, op = op), reg)
    expect_false(res$ok)
    expect_match(res$class %||% "", "^tr_error_")
  }
})

test_that("valores inválidos de op são recusados em vez de gravados", {
  reg <- test_registry()
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/const", id = "a"), reg)
  pos <- doc$ui$positions
  expect_error(tr_doc_apply(doc, list(op = "set_seed", node = "a", value = "abc"), reg),
               class = "tr_error_bad_op")
  expect_error(tr_doc_apply(doc, list(op = "move", node = "a", x = 1), reg),
               class = "tr_error_bad_op")
  expect_error(tr_doc_apply(doc, list(op = "set_param", node = "a", name = "value", value = "abc"), reg),
               class = "tr_error_bad_param_value")
  expect_identical(doc$ui$positions, pos)
})

test_that("id de instância vindo do cliente é validado", {
  reg <- test_registry()
  expect_error(tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/const", id = "a:b"), reg),
               class = "tr_error_bad_id")
})

test_that("remover o último nó de uma coleção limpa o manifesto", {
  reg <- test_registry()
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/const", id = "a"), reg)
  expect_equal(doc$collections$t, "1.0.0")
  doc <- tr_doc_apply(doc, list(op = "remove_node", node = "a"), reg)
  expect_length(doc$collections, 0)
})

test_that("validação pega ciclo, porta sobrecarregada e input obrigatório solto", {
  reg <- test_registry()
  doc <- tr_doc_parse(paste0(
    '{"format":1,"nodes":{',
    '"p":{"type":"t/add","params":{}},"q":{"type":"t/add","params":{}},',
    '"c1":{"type":"t/const","params":{}},"c2":{"type":"t/const","params":{}}},',
    '"edges":[',
    '{"from":{"node":"p","port":"out"},"to":{"node":"q","port":"a"}},',
    '{"from":{"node":"q","port":"out"},"to":{"node":"p","port":"a"}},',
    '{"from":{"node":"c1","port":"out"},"to":{"node":"q","port":"b"}},',
    '{"from":{"node":"c2","port":"out"},"to":{"node":"q","port":"b"}}]}'))
  kinds <- vapply(tr_doc_validate(doc, reg), function(p) p$kind, "")
  expect_true("cycle" %in% kinds)
  expect_true("port_overloaded" %in% kinds)
  expect_true("missing_required_input" %in% kinds)   # p.b nunca foi ligado
})

test_that("catálogo em JSON: choices é sempre array, ausência some", {
  reg <- tr_registry()
  tr_use(tr_collection(id = "t", types = list(tr_type("t/num")),
                       nodes = list(tr_node("t/e", fn = function(m) m,
                                            description = "Devolve a opção escolhida no enum.",
                                            outputs = list(out = "t/num"),
                                            params = list(m = tr_param_enum("um", c("um")))))),
         registry = reg)
  j <- jsonlite::fromJSON(tr_catalog_json(reg), simplifyVector = FALSE)
  p <- j$nodes[[1]]$params[[1]]
  expect_true(is.list(p$choices))
  expect_length(p$choices, 1)
  expect_false("label" %in% names(p))   # ausência não vira {}
})

test_that("nó sem params serializa como objeto, não array", {
  reg <- test_registry()
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/const", id = "a"), reg)
  expect_match(as.character(tr_doc_json(doc)), '"params"\\s*:\\s*\\{\\}')
})
