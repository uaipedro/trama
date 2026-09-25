# Templates: trecho de flow empacotado pra colar em outro canvas.

doc_soma <- function(reg) {
  doc <- add(tr_doc(), reg, "t/const", id = "a", params = list(value = 7), position = c(100, 50))
  doc <- add(doc, reg, "t/add", id = "s", position = c(400, 50))
  tr_doc_apply(doc, list(op = "connect", from_node = "a", from_port = "out",
                         to_node = "s", to_port = "a"), reg)
}

test_that("template faz ida-e-volta preservando nós, arestas e metadados", {
  reg <- test_registry()
  tpl <- tr_template(doc_soma(reg), nome = "Soma", descricao = "exemplo", registry = reg)
  back <- tr_template_parse(tr_template_json(tpl))
  expect_equal(back$nome, "Soma")
  expect_equal(back$descricao, "exemplo")
  expect_equal(back$doc$nodes$a$params$value, 7)
  expect_length(back$doc$edges, 1)
  # Posições normalizadas: o canto superior esquerdo do grupo vai pra (0,0).
  expect_equal(back$doc$ui$positions$a, c(0, 0))
  expect_equal(back$doc$ui$positions$s, c(300, 0))
})

test_that("JSON sem a marca não é template", {
  expect_false(tr_is_template('{"format":1,"nodes":{},"edges":[]}'))
  expect_false(tr_is_template('[1,2,3]'))
  expect_false(tr_is_template('não é json'))
  expect_true(tr_is_template('{"trama":"template","versao":1,"nome":"x","doc":{"format":1,"nodes":{},"edges":[]}}'))
  expect_error(tr_template_parse('{"format":1,"nodes":{},"edges":[]}'), class = "tr_error_not_template")
})

test_that("versão desconhecida falha alto", {
  expect_error(tr_template_parse('{"trama":"template","versao":9,"nome":"x","doc":{"format":1,"nodes":{},"edges":[]}}'),
               class = "tr_error_bad_format")
})

test_that("exportar zera params de arquivo e preserva o resto", {
  reg <- test_registry()
  tr_use(tr_collection(
    id = "d", version = "1.0.0", label = "Dados",
    nodes = list(tr_node("d/ler", fn = function(path, n) NULL, description = "Lê um arquivo.", outputs = list(out = "t/num"),
                         params = list(path = tr_param("path", ""), n = tr_param_num(1))))
  ), registry = reg)
  doc <- add(tr_doc(), reg, "d/ler", id = "l", params = list(path = "data/vendas.csv", n = 3))
  tpl <- tr_template(doc, nome = "Ler", registry = reg)
  expect_equal(tpl$doc$nodes$l$params$path, "")
  expect_equal(tpl$doc$nodes$l$params$n, 3)
})

test_that("nó de tipo fora do registro passa intocado", {
  reg <- test_registry()
  doc <- add(tr_doc(), reg, "t/const", id = "a", params = list(value = 7))
  doc$nodes$z <- list(type = "x/sumiu", label = "?", params = list(path = "a.csv"), seed = 1L)
  tpl <- tr_template(doc, nome = "X", registry = reg)
  expect_equal(tpl$doc$nodes$z$params$path, "a.csv")
  expect_equal(tpl$doc$nodes$a$params$value, 7)
})

test_that("recorte mantém só os nós pedidos e as arestas internas", {
  reg <- test_registry()
  doc <- add(doc_soma(reg), reg, "t/add", id = "t", position = c(700, 50))
  doc <- tr_doc_apply(doc, list(op = "connect", from_node = "s", from_port = "out",
                                to_node = "t", to_port = "a"), reg)
  doc <- tr_doc_apply(doc, list(op = "add_frame", id = "f", x = 0, y = 0, w = 100, h = 100), reg)
  doc <- tr_doc_apply(doc, list(op = "add_note", id = "n1", x = 0, y = 0, w = 50, h = 50,
                                kind = "markdown", text = "oi"), reg)
  doc <- tr_doc_apply(doc, list(op = "add_note", id = "n2", x = 9, y = 9, w = 50, h = 50,
                                kind = "markdown"), reg)
  sub <- tr_doc_subset(doc, c("a", "s", "n1"))
  expect_setequal(names(sub$nodes), c("a", "s"))
  expect_length(sub$edges, 1)
  expect_equal(names(sub$ui$positions), c("a", "s"))
  expect_length(sub$ui$frames, 0)
  expect_equal(names(sub$ui$notes), "n1")
  expect_equal(names(sub$collections), "t")
  expect_identical(tr_doc_subset(doc), doc)
})
