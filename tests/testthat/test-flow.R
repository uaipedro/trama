test_that("a DSL produz o MESMO documento que as ops", {
  reg <- store_registry()
  via_ops <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "a", params = list(v = 2)),
    list(op = "add_node", type = "t/inc",   id = "b", params = list(by = 3)),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "b", to_port = "x")))
  via_dsl <- tr_flow(reg) |>
    tr_add("a", "t/const", v = 2) |>
    tr_add("b", "t/inc", by = 3, from = "a") |>
    tr_flow_doc()
  strip <- function(d) { d$rev <- NULL; d$nodes <- lapply(d$nodes, function(n) { n$seed <- NULL; n }); d }
  expect_equal(strip(via_dsl), strip(via_ops))
})

test_that("from liga várias origens a uma porta variádica, na ordem", {
  reg <- store_registry(); s <- tmp_store()
  f <- tr_flow(reg) |>
    tr_add("a", "t/const", v = 1) |> tr_add("b", "t/const", v = 10) |>
    tr_add("s", "t/sum", from = c("a", "b"))
  expect_equal(tr_value(f, "s", registry = reg, store = s)$v, 11)
})

test_that("tr_link com porta explícita e tr_set", {
  reg <- store_registry(); s <- tmp_store()
  f <- tr_flow(reg) |> tr_add("a", "t/const", v = 1) |> tr_add("b", "t/inc") |>
    tr_link("a:out", "b:x") |> tr_set("b", by = 41)
  expect_equal(tr_value(f, "b", registry = reg, store = s)$v, 42)
})

test_that("tr_set() denuncia nome que o partial matching desviaria pra flow/id", {
  # Achado da pauta pós-região de fluxo, repro exato: `f` é prefixo único de
  # `flow`, e o R o casa por partial matching ANTES de distribuir os
  # argumentos posicionais — sem a guarda, `tr_set(f, "b", f = 41)` devolvia
  # `41` em vez de um fluxo, sem erro em lugar nenhum.
  reg <- store_registry(); s <- tmp_store()
  f <- tr_flow(reg) |> tr_add("a", "t/const", v = 1) |> tr_add("b", "t/inc") |>
    tr_link("a:out", "b:x")
  expect_error(tr_set(f, "b", f = 41), class = "tr_error_param_shadow")
  expect_error(tr_set(f, "b", i = 41), class = "tr_error_param_shadow")

  # nome que não é prefixo de flow/id continua funcionando normalmente
  expect_equal(tr_value(tr_set(f, "b", by = 41), "b", registry = reg, store = s)$v, 42)
})

test_that("from sem porta compatível é erro alto, não ligação silenciosa", {
  reg <- store_registry()
  # `t/const` não tem porta de entrada nenhuma — não há como `from` encontrar
  # onde ligar, e o erro precisa dizer isso, não ligar em silêncio.
  expect_error(tr_flow(reg) |> tr_add("a", "t/const") |> tr_add("c", "t/const", from = "a"),
               class = "tr_error_type_mismatch")
})

test_that("tr_flow_code(doc) avaliado reconstrói um documento com as MESMAS chaves", {
  reg <- store_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "t/const", id = "a", params = list(v = 2)),
    list(op = "add_node", type = "t/const", id = "b", params = list(v = 5)),
    list(op = "add_node", type = "t/inc",   id = "i", params = list(by = 3)),
    list(op = "add_node", type = "t/sum",   id = "s"),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "i", to_port = "x"),
    list(op = "connect", from_node = "i", from_port = "out", to_node = "s", to_port = "xs"),
    list(op = "connect", from_node = "b", from_port = "out", to_node = "s", to_port = "xs")))
  code <- tr_flow_code(doc, reg)
  expect_match(code, "tr_add\\(\"i\", \"t/inc\", by = 3, from = \"a\"\\)")
  reg <- reg   # nome que o código gerado usa
  doc2 <- eval(parse(text = code)) |> tr_flow_doc()
  expect_equal(tr_plan(doc2, registry = reg)$keys[names(doc$nodes)], tr_plan(doc, registry = reg)$keys[names(doc$nodes)])
})

test_that("o ETL de exemplo vira código legível e volta igual", {
  skip_if_no_data()
  # Migrado como o editor abre: o JSON do exemplo ainda liga as arestas na
  # porta antiga `data`, e o código gerado usa as portas do registro atual.
  reg <- etl_registry()
  doc <- tr_doc_migrate(tr_doc_read("../../exemplos/vendas/flows/main.json"), reg)
  code <- tr_flow_code(doc, reg)
  doc2 <- eval(parse(text = code)) |> tr_flow_doc()
  expect_equal(sort(names(doc2$nodes)), sort(names(doc$nodes)))
  expect_equal(length(doc2$edges), length(doc$edges))
})

test_that("param de nó que colide com argumento de tr_add() falha alto", {
  # `from` é o caso perigoso: não falha, faz outra coisa. Sem esta checagem,
  # `tr_add(..., from = "x")` num nó cujo param se chama `from` liga uma aresta
  # a partir de `x` e deixa o param por preencher — calado, se `x` existir.
  reg <- tr_registry()
  tr_use(tr_collection("z", types = list(tr_type("z/t")), nodes = list(
    tr_node("z/fonte", fn = function() 1, description = "Fonte.", outputs = list(out = "z/t")),
    tr_node("z/ren", fn = function(data, from) data, description = "Tem param 'from'.",
            inputs = list(data = "z/t"), outputs = list(out = "z/t"),
            params = list(from = tr_param_text("a", label = "De")))
  )), registry = reg)

  f <- tr_flow(reg) |> tr_add("fonte", "z/fonte")
  err <- tryCatch(tr_add(f, "r", "z/ren", from = "fonte"), error = identity)
  expect_equal(class(err)[[1]], "tr_error_param_shadow")
  expect_match(conditionMessage(err), "tr_set")

  # O nó continua utilizável: o param vai por tr_set(), a aresta por tr_link().
  ok <- f |> tr_add("r", "z/ren") |> tr_link("fonte", "r") |> tr_set("r", from = "regiao")
  expect_equal(tr_flow_doc(ok)$nodes$r$params$from, "regiao")
})

# O gerador é a outra metade da mesma armadilha: se ele emite o param `from`
# DENTRO do `tr_add()`, sai `tr_add("r", "z/ren", from = "mpg", from = "fonte")`
# — argumento duplicado, código que nem faz parse. E `type` é pior ainda: o R
# casa o nome com o formal `type` de `tr_add()` e o tipo do nó fica sem posição.
# O round-trip é a prova: gerar, AVALIAR o gerado, e comparar as chaves de plano.
shadow_registry <- function() {
  reg <- tr_registry()
  tr_use(tr_collection("z", types = list(tr_type("z/t")), nodes = list(
    tr_node("z/fonte", fn = function() 1, description = "Fonte.", outputs = list(out = "z/t")),
    tr_node("z/ren", fn = function(data, from, to) data, description = "Tem param 'from'.",
            inputs = list(data = "z/t"), outputs = list(out = "z/t"),
            params = list(from = tr_param_text("", label = "De"),
                          to   = tr_param_text("", label = "Para"))),
    tr_node("z/junta", fn = function(data, type) data, description = "Tem param 'type'.",
            inputs = list(data = "z/t"), outputs = list(out = "z/t"),
            params = list(type = tr_param_text("inner", label = "Tipo")))
  )), registry = reg)
  reg
}

test_that("tr_flow_code manda param de nome reservado pra tr_set, e o gerado roda", {
  reg <- shadow_registry()
  doc <- tr_flow(reg) |>
    tr_add("fonte", "z/fonte") |>
    tr_add("r", "z/ren") |> tr_link("fonte", "r") |> tr_set("r", from = "mpg", to = "consumo") |>
    tr_add("j", "z/junta") |> tr_link("r", "j") |> tr_set("j", type = "left") |>
    tr_flow_doc()
  code <- tr_flow_code(doc, reg)

  # Nenhum param reservado dentro do tr_add() — nem duplicado, nem sozinho.
  expect_no_match(code, "tr_add\\([^)]*\\bfrom = \"mpg\"")
  expect_no_match(code, "tr_add\\([^)]*\\btype = \"left\"")
  expect_match(code, 'tr_set\\("r", from = "mpg"\\)', fixed = FALSE)
  expect_match(code, 'tr_set\\("j", type = "left"\\)', fixed = FALSE)

  doc2 <- eval(parse(text = code)) |> tr_flow_doc()
  expect_equal(tr_plan(doc2, registry = reg)$keys[names(doc$nodes)],
               tr_plan(doc, registry = reg)$keys[names(doc$nodes)])
  expect_equal(tr_flow_code(doc2, reg), code)
})

test_that("tr_export_code gera um script R executável sem a DSL do trama", {
  reg <- tr_registry()
  tr_use(tr_collection("e", types = list(tr_type("e/num"), tr_type("e/txt")), nodes = list(
    tr_node("e/fonte", fn = function(valor) valor, description = "Fonte.",
            outputs = list(out = "e/num"), params = list(valor = tr_param_num(1))),
    tr_node("e/divide", fn = function(x) list(esquerda = x, direita = x + 1),
            description = "Duas saídas.", inputs = list(x = "e/num"),
            outputs = list(esquerda = "e/num", direita = "e/num")),
    tr_node("e/soma", fn = function(a, b, extra) a + b + extra, description = "Soma.",
            inputs = list(a = "e/num", b = "e/num"), outputs = list(out = "e/num"),
            params = list(extra = tr_param_num(0))),
    tr_node("e/texto", fn = function(x) x, description = "Texto.",
            inputs = list(x = "e/txt"), outputs = list(out = "e/txt"))
  ), adapters = list(tr_adapter("e/num", "e/txt", function(x) paste0("n=", x)))), registry = reg)

  doc <- tr_flow(reg) |>
    tr_add("origem", "e/fonte", valor = 4) |>
    tr_add("partes", "e/divide", from = "origem") |>
    tr_add("total", "e/soma", extra = 2) |>
    tr_link("partes:esquerda", "total:a") |>
    tr_link("partes:direita", "total:b") |>
    tr_add("rotulo", "e/texto") |>
    tr_link("total", "rotulo") |>
    tr_flow_doc()

  code <- tr_export_code(doc, reg)
  expect_no_match(code, "trama::")
  expect_no_match(code, "tr_flow")
  env <- new.env(parent = globalenv())
  eval(parse(text = code), envir = env)
  expect_equal(env$rotulo, "n=11")
})

test_that("tr_export_code embrulha o mesmo script em um documento Quarto", {
  reg <- test_registry()
  doc <- tr_flow(reg) |> tr_add("origem", "t/const", value = 2) |> tr_flow_doc()

  quarto <- tr_export_code(doc, reg, format = "quarto", title = "Minha análise")
  expect_match(quarto, "^---\\ntitle: \\\"Minha análise\\\"")
  expect_match(quarto, "```\\{r\\}")
  expect_no_match(quarto, "trama::")
})

# Coleção do teste do relatório: rótulos de verdade, um default de spec que
# difere do da função, um tipo com `report` e uma saída que ninguém consome.
export_registry <- function() {
  reg <- tr_registry()
  tr_use(tr_collection("x", types = list(
    tr_type("x/num"),
    tr_type("x/vis", report = function(x) paste("visto", x))
  ), nodes = list(
    tr_node("x/fonte", fn = function(valor = 1) valor, label = "Valor de partida",
            description = "Fonte.", outputs = list(out = "x/num"),
            params = list(valor = tr_param_num(1))),
    tr_node("x/escala", fn = function(x, fator = 1) x * fator, label = "Escala",
            description = "Default do spec diferente do da função.",
            inputs = list(x = "x/num"), outputs = list(out = "x/num"),
            params = list(fator = tr_param_num(10))),
    tr_node("x/ajuste", fn = function(x) list(out = x + 1, ajuste = x * 2), label = "Ajuste",
            description = "Duas saídas.", inputs = list(x = "x/num"),
            outputs = list(out = "x/num", ajuste = "x/vis"))
  )), registry = reg)
  reg
}

test_that("o script exportado usa o rótulo do card, params efetivos e uma variável por nó", {
  reg <- export_registry()
  doc <- tr_doc()
  for (op in list(
    list(op = "add_node", type = "x/fonte", id = "cmupd9cxqigl035cr", params = list(valor = 3)),
    list(op = "add_node", type = "x/escala", id = "cmupddntkae642ex1"),
    list(op = "add_node", type = "x/ajuste", id = "cmupdemxf6pxw5p6p"),
    list(op = "add_node", type = "x/escala", id = "cmupdew8afk9q1zyj"),
    list(op = "connect", from_node = "cmupd9cxqigl035cr", from_port = "out", to_node = "cmupddntkae642ex1", to_port = "x"),
    list(op = "connect", from_node = "cmupddntkae642ex1", from_port = "out", to_node = "cmupdemxf6pxw5p6p", to_port = "x"),
    list(op = "connect", from_node = "cmupdemxf6pxw5p6p", from_port = "out", to_node = "cmupdew8afk9q1zyj", to_port = "x")
  )) doc <- tr_doc_apply(doc, op, reg)

  code <- tr_export_code(doc, reg)
  expect_no_match(code, "cmupd")
  # Rótulo vira nome; repetido ganha sufixo.
  expect_match(code, "valor_de_partida <- ", fixed = TRUE)
  expect_match(code, "escala_2 <- ", fixed = TRUE)
  # Nó de várias saídas é uma variável só, lida por porta.
  expect_match(code, "(x = ajuste$out", fixed = TRUE)
  expect_no_match(code, "_resultado")
  # O default do spec (10) vai escrito: o da função (1) daria outra conta.
  expect_match(code, "fator = 10", fixed = TRUE)

  env <- new.env(parent = globalenv())
  eval(parse(text = code), envir = env)
  expect_equal(env$escala_2, (3 * 10 + 1) * 10)

  # O executor calcula o mesmo número.
  store <- tr_store(withr::local_tempdir())
  expect_equal(tr_value(doc, "cmupdew8afk9q1zyj", registry = reg, store = store), env$escala_2)
})

test_that("o Quarto tem um chunk por card, seções dos frames, notas e as saídas finais à mostra", {
  reg <- export_registry()
  doc <- tr_doc()
  for (op in list(
    list(op = "add_node", type = "x/fonte", id = "a", position = list(0, 0)),
    list(op = "add_node", type = "x/ajuste", id = "b", position = list(1200, 0)),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "b", to_port = "x"),
    list(op = "add_frame", id = "f2", x = 1000, y = -100, w = 800, h = 600, title = "Ajuste"),
    list(op = "add_frame", id = "f1", x = -100, y = -100, w = 800, h = 600, title = "Dados"),
    list(op = "reorder_frames", frames = list("f1", "f2")),
    list(op = "add_note", id = "n", x = 1100, y = -50, kind = "markdown", text = "O ajuste dobra.")
  )) doc <- tr_doc_apply(doc, op, reg)

  q <- tr_export_code(doc, reg, format = "quarto", title = "T")
  expect_match(q, "embed-resources: true", fixed = TRUE)
  pos <- function(x) regexpr(x, q, fixed = TRUE)[[1]]
  expect_true(all(c(pos("## Dados"), pos("## Ajuste"), pos("O ajuste dobra.")) > 0))
  expect_lt(pos("## Dados"), pos("valor_de_partida <-"))
  expect_lt(pos("valor_de_partida <-"), pos("## Ajuste"))
  expect_lt(pos("## Ajuste"), pos("O ajuste dobra."))
  expect_lt(pos("O ajuste dobra."), pos("ajuste <-"))
  expect_match(q, "#| label: valor-de-partida", fixed = TRUE)
  # As duas saídas de `b` ficam sem consumidor: a de tipo com `report` passa
  # por ele, a outra é impressa.
  expect_match(q, "ajuste$out\n", fixed = TRUE)
  expect_match(q, "paste\\(\"visto\", x\\)\\)\\(ajuste\\$ajuste\\)")
  expect_match(q, "sessionInfo()", fixed = TRUE)
})
