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
  reg <- etl_registry(); doc <- tr_doc_read("../../exemplos/vendas/flows/main.json")
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
