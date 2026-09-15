# O tipo `data/table` promete uma TABELA. Sem guard, o `store` aceitava
# qualquer objeto e o `preview` desenhava uma tabela plausível em cima dele:
# `1:10` virava uma coluna `x`, um `lm` virava uma tabela de zero colunas, e o
# card ficava verde. O erro só aparecia um nó adiante, longe da causa — que é
# o pior modo de falha desta ferramenta.
#
# O guard mora no `store` porque ele é o funil único por onde todo valor entra
# no artefato: cobre qualquer nó que declare `data/table`, hoje e amanhã, sem
# que cada `fn` precise lembrar.

test_that("store do data/table recusa objeto que não é tabela, classificado", {
  st <- data_table_type()$store
  p <- tempfile(fileext = ".rds")

  for (x in list(1:10, list(a = 1, b = "x"), stats::lm(mpg ~ cyl, datasets::mtcars))) {
    expect_error(st(x, p), class = "tr_data_error_not_a_table")
    e <- tryCatch(st(x, p), error = identity)
    # Primeira ordem: é `class(e)[1]` que o motor grava no handle de erro.
    expect_equal(class(e)[[1]], "tr_data_error_not_a_table")
    expect_match(conditionMessage(e), class(x)[[1]], fixed = TRUE)
  }
  expect_false(file.exists(p))
})

test_that("store do data/table aceita data.frame e tibble", {
  st <- data_table_type()$store
  p <- tempfile(fileext = ".rds")
  expect_no_error(st(df_exemplo(), p))
  expect_no_error(st(as.data.frame(df_exemplo()), p))
  expect_equal(readRDS(p), as.data.frame(df_exemplo()))
})

# A prova de ponta a ponta, no motor de verdade: um `.rds` com um `lm` dentro
# lido por `data/read_rds`. Antes, isto passava verde com preview de tabela de
# zero colunas.
test_that("nó que produz não-tabela falha no PRÓPRIO nó, no motor", {
  reg <- data_registry()
  s <- trama::tr_store(tempfile("store"))
  p <- tempfile(fileext = ".rds")
  saveRDS(stats::lm(mpg ~ cyl, datasets::mtcars), p)

  doc <- trama::tr_doc_apply(trama::tr_doc(),
    list(op = "add_node", type = "data/read_rds", id = "ler",
         params = list(path = p)), reg)
  ev <- list()
  trama::tr_run(doc, registry = reg, store = s,
                on_event = function(e) ev[[length(ev) + 1]] <<- e)
  falhas <- Filter(function(e) identical(e$type, "failed"), ev)
  expect_length(falhas, 1L)
  expect_equal(falhas[[1]]$node, "ler")
  expect_equal(falhas[[1]]$class, "tr_data_error_not_a_table")
})
