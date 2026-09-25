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
