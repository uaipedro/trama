test_that("documento vazio faz round-trip com nodes como objeto, não array", {
  doc <- tr_doc()
  txt <- tr_doc_json(doc)
  expect_match(as.character(txt), '"nodes"\\s*:\\s*\\{\\}')
  back <- tr_doc_parse(txt)
  # Objeto JSON vazio volta como lista NOMEADA vazia, não `list()` cru — é o
  # que garante que `back$nodes[["algum_id"]]` devolva NULL em vez de erro.
  expect_length(back$nodes, 0)
  expect_null(back$nodes[["qualquer"]])
  expect_s3_class(back, "tr_doc")
})

test_that("round-trip preserva semântica e apresentação", {
  reg <- test_registry()
  doc <- tr_doc_apply(tr_doc(), list(op = "add_node", type = "t/const", id = "a",
                                     params = list(value = 7), position = c(10, 20)), reg)
  doc <- tr_doc_apply(doc, list(op = "add_node", type = "t/add", id = "s"), reg)
  doc <- tr_doc_apply(doc, list(op = "connect", from_node = "a", from_port = "out",
                                to_node = "s", to_port = "a"), reg)
  back <- tr_doc_parse(tr_doc_json(doc))
  expect_equal(back$nodes$a$params$value, 7)
  expect_equal(back$nodes$a$seed, doc$nodes$a$seed)
  # Array JSON de escalares homogêneos volta como VETOR, não lista — é a
  # correção que impede a chave de conteúdo de mudar entre salvar e reabrir.
  expect_equal(back$ui$positions$a, c(10, 20))
  expect_length(back$edges, 1)
  expect_equal(back$edges[[1]]$to$port, "a")
})

test_that("formato desconhecido falha alto e cedo", {
  expect_error(tr_doc_parse('{"format": 99}'), class = "tr_error_bad_format")
})

test_that("validação reporta tipo desconhecido como órfão, sem falhar", {
  reg <- test_registry()
  doc <- tr_doc_parse('{"format":1,"rev":1,"nodes":{"a":{"type":"t/sumiu","params":{}}},"edges":[]}')
  probs <- tr_doc_validate(doc, reg)
  expect_length(probs, 1)
  expect_equal(probs[[1]]$kind, "unknown_node_type")
})

test_that("validação detecta aresta pendurada e deriva de versão", {
  reg <- test_registry()
  doc <- tr_doc_parse(paste0(
    '{"format":1,"nodes":{"a":{"type":"t/const","type_version":99,"params":{}}},',
    '"edges":[{"from":{"node":"a","port":"out"},"to":{"node":"x","port":"a"}}]}'))
  kinds <- vapply(tr_doc_validate(doc, reg), function(p) p$kind, "")
  expect_true("dangling_edge" %in% kinds)
  expect_true("version_drift" %in% kinds)
})

test_that("tamanho e vista sobrevivem à ida e volta pelo JSON", {
  x <- mk(); d0 <- add(x$doc, x$reg, "t/const", id = "a")
  doc <- tr_doc_apply(d0, list(op = "resize", node = "a", w = 480, h = 300), x$reg)
  doc <- tr_doc_apply(doc, list(op = "set_view", node = "a", view = "resumo"), x$reg)
  back <- tr_doc_parse(tr_doc_json(doc))
  expect_equal(back$ui$sizes[["a"]], c(480, 300))
  expect_equal(back$ui$views[["a"]], "resumo")
})

test_that("documento sem ui.sizes/ui.views abre e sai como objeto, não array", {
  back <- tr_doc_parse('{"format":1,"nodes":{},"edges":[]}')
  expect_identical(back$ui$sizes, stats::setNames(list(), character(0)))
  expect_match(as.character(tr_doc_json(back)), '"sizes"\\s*:\\s*\\{\\}')
  expect_identical(back$ui$frames, stats::setNames(list(), character(0)))
  expect_identical(back$ui$modes, stats::setNames(list(), character(0)))
  expect_identical(back$ui$notes, stats::setNames(list(), character(0)))
})

# Documento recém-criado nunca passou pelo parse, e é o único caminho que
# exercita o `.tr_empty_obj` do lado da ESCRITA — sem esta asserção, tirar as
# três linhas de `tr_doc_json` passa despercebido pela suíte inteira.
test_that("documento novo serializa os mapas de ui como objeto", {
  j <- as.character(tr_doc_json(tr_doc()))
  for (k in c("positions", "sizes", "views", "frames", "modes", "notes")) {
    expect_match(j, sprintf('"%s"\\s*:\\s*\\{\\}', k))
  }
})

test_that("frames e modes sobrevivem à ida e volta pelo JSON", {
  m <- mk(); d <- add(m$doc, m$reg, "t/const", id = "a")
  d <- tr_doc_apply(d, list(op = "add_frame", id = "f", x = -10, y = 5.5, w = 1600, h = 900,
                            title = "Leitura"), m$reg)
  d <- tr_doc_apply(d, list(op = "set_mode", node = "a", modo = "params"), m$reg)
  back <- tr_doc_parse(tr_doc_json(d))
  expect_equal(back$ui$frames$f[c("x", "y", "w", "h", "title", "aspect", "color", "order")],
               list(x = -10, y = 5.5, w = 1600, h = 900, title = "Leitura",
                    aspect = "16:9", color = "azul", order = 1L))
  expect_identical(back$ui$modes$a, "params")
})

test_that("folds antigos viram modes na leitura", {
  js <- '{"format":1,"nodes":{},"edges":[],"ui":{"folds":{
    "a":{"mini":true},
    "b":{"preview":false},
    "c":{"params":false},
    "d":{"preview":false,"params":false},
    "e":{}
  }}}'
  d <- tr_doc_parse(js)
  expect_identical(d$ui$modes$a, "mini")
  expect_identical(d$ui$modes$b, "params")
  expect_identical(d$ui$modes$c, "preview")
  expect_identical(d$ui$modes$d, "mini")
  expect_null(d$ui$modes$e)
  expect_null(d$ui$folds)
})

test_that("modes explícito vence folds antigo do mesmo nó", {
  js <- '{"format":1,"nodes":{},"edges":[],"ui":{"folds":{"a":{"mini":true}},"modes":{"a":"preview"}}}'
  expect_identical(tr_doc_parse(js)$ui$modes$a, "preview")
})

test_that("notas sobrevivem à ida e volta pelo JSON", {
  m <- mk()
  d <- tr_doc_apply(m$doc, list(op = "add_note", id = "n", x = -10, y = 5.5,
                                w = 320, h = 120, kind = "markdown",
                                text = "# Método\n\n- um\n- dois", color = "rosa",
                                escala = "letreiro", fundo = "nenhum"), m$reg)
  d <- tr_doc_apply(d, list(op = "add_note", id = "i", x = 0, y = 0, w = 400, h = 300,
                            kind = "imagem", src = "figuras/logo.png", fit = "cover"), m$reg)
  back <- tr_doc_parse(tr_doc_json(d))
  expect_equal(back$ui$notes$n[c("x", "y", "w", "h", "kind", "text", "escala", "fundo", "color")],
               d$ui$notes$n[c("x", "y", "w", "h", "kind", "text", "escala", "fundo", "color")])
  expect_equal(back$ui$notes$i$src, "figuras/logo.png")
  expect_equal(back$ui$notes$i$fit, "cover")
})
