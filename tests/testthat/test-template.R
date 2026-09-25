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

test_that("ops do template inserem com ids novos e na origem pedida", {
  reg <- test_registry()
  doc <- tr_doc_apply(doc_soma(reg), list(op = "resize", node = "s", w = 320, h = 200), reg)
  doc <- tr_doc_apply(doc, list(op = "set_mode", node = "s", modo = "mini"), reg)
  doc <- tr_doc_apply(doc, list(op = "add_frame", id = "f", x = 100, y = 50, w = 600, h = 300,
                                title = "Grupo"), reg)
  doc <- tr_doc_apply(doc, list(op = "add_note", id = "n", x = 150, y = 300, w = 80, h = 40,
                                kind = "markdown", text = "oi"), reg)
  tpl <- tr_template(doc, nome = "Soma", registry = reg)
  base <- add(tr_doc(), reg, "t/const", id = "a")
  op <- tr_template_op(tpl, origin = c(1000, 500))
  expect_equal(op$op, "batch")
  res <- tr_doc_apply(base, op, reg)
  expect_length(res$nodes, 3)                     # "a" original + 2 do template
  expect_length(res$edges, 1)
  novos <- setdiff(names(res$nodes), "a")
  tipos <- vapply(res$nodes[novos], `[[`, "", "type")
  const <- novos[tipos == "t/const"]; soma <- novos[tipos == "t/add"]
  expect_equal(res$ui$positions[[const]], c(1000, 500))
  expect_equal(res$ui$positions[[soma]], c(1300, 500))
  expect_equal(res$nodes[[const]]$params$value, 7)
  expect_equal(res$ui$sizes[[soma]], c(320, 200))
  expect_equal(res$ui$modes[[soma]], "mini")
  expect_equal(res$edges[[1]]$from$node, const)
  expect_equal(res$edges[[1]]$to$node, soma)
  f <- res$ui$frames[[1]]
  expect_equal(c(f$x, f$y, f$w, f$h), c(1000, 500, 600, 300))
  expect_equal(f$title, "Grupo")
  n <- res$ui$notes[[1]]
  expect_equal(c(n$x, n$y), c(1050, 750))
  expect_equal(n$text, "oi")
  # Colar de novo não colide.
  res2 <- tr_doc_apply(res, tr_template_op(tpl, c(0, 0)), reg)
  expect_length(res2$nodes, 5)
  expect_length(res2$edges, 2)
  # E o template lido do disco insere igual ao recém-criado.
  lido <- tr_template_parse(tr_template_json(tpl))
  res3 <- tr_doc_apply(base, tr_template_op(lido, c(1000, 500)), reg)
  expect_equal(unname(res3$ui$positions[setdiff(names(res3$nodes), "a")]),
               unname(res$ui$positions[novos]))
})

test_that("gravar e listar: biblioteca e projeto, sem sobrescrever calado", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir())
  root <- withr::local_tempdir()
  reg <- test_registry()
  tpl <- tr_template(doc_soma(reg), nome = "Soma Básica", descricao = "d", registry = reg)

  path <- tr_template_save(tpl, tr_template_dir("biblioteca"))
  expect_equal(basename(path), "soma-basica.json")
  expect_error(tr_template_save(tpl, tr_template_dir("biblioteca")), class = "tr_error_template_exists")
  expect_no_error(tr_template_save(tpl, tr_template_dir("biblioteca"), overwrite = TRUE))

  tr_template_save(tpl, tr_template_dir("projeto", root))
  # Arquivo inválido na pasta é ignorado, não derruba a listagem.
  writeLines("{ quebrado", file.path(tr_template_dir("projeto", root), "ruim.json"))
  writeLines('{"format":1,"nodes":{},"edges":[]}', file.path(tr_template_dir("projeto", root), "dados.json"))

  lst <- tr_template_list(root, registry = reg)
  expect_length(lst, 2)
  expect_setequal(vapply(lst, `[[`, "", "escopo"), c("biblioteca", "projeto"))
  expect_true(all(vapply(lst, function(x) x$escopo %in% c("colecao", "biblioteca", "projeto"), logical(1))))
  expect_equal(lst[[1]]$nome, "Soma Básica")
  expect_equal(lst[[1]]$descricao, "d")
  expect_true(file.exists(lst[[1]]$arquivo))
  expect_true(is.character(lst[[1]]$colecoes))
})

test_that("slug cai em 'template' quando o nome não tem nada aproveitável", {
  expect_equal(.tr_slug("Ação & Reação!"), "acao-reacao")
  expect_equal(.tr_slug("!!!"), "template")
})

test_that("versao com mais de um valor é rejeitada sem estourar a mensagem", {
  expect_error(tr_template_parse('{"trama":"template","versao":[1,2],"doc":{}}'),
               class = "tr_error_bad_format", regexp = "1,2")
})

test_that("aresta para nó inexistente é rejeitada no parse", {
  txt <- '{"trama":"template","versao":1,"nome":"x","doc":{"format":1,"nodes":{"a":{"type":"x","label":"a","params":{}}},
    "edges":[{"from":{"node":"a","port":"out"},"to":{"node":"zz","port":"in"}}]}}'
  expect_error(tr_template_parse(txt), class = "tr_error_bad_format", regexp = "zz")
})

test_that("slug aceita nome vazio ou vetor", {
  expect_equal(.tr_slug(character()), "template")
  expect_equal(.tr_slug(c("Um", "Dois")), "um")
})
