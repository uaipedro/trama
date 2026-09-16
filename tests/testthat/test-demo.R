# A coleção `demo` é a única que mora no núcleo, e ela existe para que o pacote
# instalado faça algo sozinho — sem coleção externa não há bloco, não há fluxo e
# não há exemplo executável na ajuda. Este arquivo guarda essa promessa.
#
# A coleção de teste de `helper-collection.R` continua existindo, e continua
# sendo a guarda contra acoplamento: ela prova que o núcleo roda sem NENHUMA
# coleção real. A `demo` é coleção de verdade, ainda que de brinquedo, e por isso
# é testada aqui, à parte.

demo_registry <- function() {
  reg <- tr_registry()
  tr_use("trama", registry = reg)
  reg
}

test_that("tr_use('trama') registra a coleção demo num registro limpo", {
  reg <- demo_registry()

  expect_named(reg$collections, "demo")
  expect_setequal(names(reg$nodes),
                  c("demo/const", "demo/soma", "demo/tabela",
                    "demo/filtrar", "demo/resumo"))
  expect_setequal(names(reg$types), c("demo/num", "demo/tabela"))
  expect_setequal(names(reg$categories), c("numeros", "tabelas"))
  # `package` é o nome pelo qual a coleção foi carregada; `id` é o namespace dos
  # blocos. Aqui os dois DIFEREM, e é essa diferença que dispensa caso especial
  # em `tr_use()`.
  expect_equal(reg$collections$demo$package, "trama")
})

test_that("demo/const -> demo/soma roda de ponta a ponta", {
  reg <- demo_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "demo/const", id = "a", params = list(value = 2)),
    list(op = "add_node", type = "demo/const", id = "b", params = list(value = 3)),
    list(op = "add_node", type = "demo/soma",  id = "s", params = list(k = 10)),
    list(op = "connect", from_node = "a", from_port = "out", to_node = "s", to_port = "a"),
    list(op = "connect", from_node = "b", from_port = "out", to_node = "s", to_port = "b")))

  expect_equal(tr_value(doc, "s", reg, s), 15)
  # Nível 1: a função do bloco é uma função R comum, e dá o mesmo número.
  expect_equal(tr_fn("demo/soma", reg)(2, 3, 10), 15)
})

test_that("demo/tabela -> demo/filtrar devolve as linhas esperadas", {
  reg <- demo_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "demo/tabela", id = "t"),
    list(op = "add_node", type = "demo/filtrar", id = "f",
         params = list(column = "lados", limit = 4)),
    list(op = "connect", from_node = "t", from_port = "out", to_node = "f", to_port = "data")))

  tudo <- tr_value(doc, "t", reg, s)
  expect_equal(nrow(tudo), 6L)
  # Das seis figuras, só o triângulo tem menos de quatro lados.
  expect_equal(nrow(tr_value(doc, "f", reg, s)), 5L)
})

test_that("demo/resumo devolve uma tabela com as colunas numéricas da entrada", {
  reg <- demo_registry(); s <- tmp_store()
  doc <- build(reg, list(
    list(op = "add_node", type = "demo/tabela", id = "t"),
    list(op = "add_node", type = "demo/resumo", id = "r"),
    list(op = "connect", from_node = "t", from_port = "out", to_node = "r", to_port = "data")))

  res <- tr_value(doc, "r", reg, s)
  expect_named(res, c("estatistica", "lados", "area"))
  expect_equal(nrow(res), 6L)   # as seis estatísticas de summary()
})

test_that("os previews dos dois tipos produzem artefato válido", {
  reg <- demo_registry()

  pn <- tr_get_type("demo/num", reg)$preview(7, NULL)
  expect_equal(pn$renderer, "trama/keyvalue")
  expect_equal(pn$data$valor, 7)

  tabela <- tr_fn("demo/tabela", reg)()
  pt <- tr_get_type("demo/tabela", reg)$preview(tabela, NULL)
  expect_equal(pt$renderer, "trama/table")
  expect_equal(pt$data$columns, list("figura", "lados", "area"))
  expect_length(pt$data$rows, 6L)
  expect_equal(pt$data$nrow, 6L)
  # O preview tem que virar JSON: é assim que ele chega ao front.
  expect_type(jsonlite::toJSON(pt, auto_unbox = TRUE), "character")

  expect_equal(tr_get_type("demo/tabela", reg)$summary(tabela),
               list(linhas = 6L, colunas = 3L))
})

test_that("a coleção demo carrega junto de outra sem colidir", {
  # O id `demo` é curto e genérico, e o teste que importa é que ele conviva:
  # `t` (coleção de teste) e `demo` no mesmo registro, sem duplicidade.
  reg <- demo_registry()
  tr_use(test_collection(), registry = reg)
  expect_setequal(names(reg$collections), c("demo", "t"))
})
