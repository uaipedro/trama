test_that("catálogo carrega tudo que o front precisa e nada de domínio", {
  cat <- tr_catalog(test_registry())

  expect_equal(cat$schema_version, 1L)
  expect_setequal(vapply(cat$types, function(t) t$id, ""), c("t/num", "t/txt"))
  expect_setequal(vapply(cat$nodes, function(n) n$id, ""), c("t/const", "t/add", "t/show"))
  expect_equal(cat$adapters[[1]], list(from = "t/num", to = "t/txt"))
  expect_equal(cat$categories[[1]]$label, "Básico")
  expect_equal(cat$collections[[1]]$version, "1.0.0")

  add <- Filter(function(n) n$id == "t/add", cat$nodes)[[1]]
  expect_equal(vapply(add$inputs, function(p) p$name, ""), c("a", "b"))
  expect_equal(add$inputs[[1]]$type, "t/num")
  expect_true(add$inputs[[1]]$required)
  expect_equal(add$params[[1]]$name, "k")
  expect_equal(add$params[[1]]$kind, "number")
})

test_that("catálogo é serializável em JSON sem perder forma", {
  cat <- tr_catalog(test_registry())
  round <- jsonlite::fromJSON(jsonlite::toJSON(cat, auto_unbox = TRUE), simplifyVector = FALSE)
  expect_equal(round$schema_version, 1L)
  expect_length(round$nodes, 3)
})

test_that("porta variádica aparece no catálogo", {
  col <- tr_collection(
    id = "t", types = list(tr_type("t/num")),
    nodes = list(tr_node("t/merge", fn = function(xs) xs,
                         description = "Junta numa lista todas as entradas ligadas.",
                         inputs = list(xs = tr_port("t/num", multiple = TRUE)),
                         outputs = list(out = "t/num")))
  )
  reg <- tr_registry(); tr_use(col, registry = reg)
  n <- tr_catalog(reg)$nodes[[1]]
  expect_true(n$inputs[[1]]$multiple)
})

test_that("help viaja no catálogo, e some quando ausente", {
  reg <- tr_registry()
  tr_use(tr_collection("x", types = list(tr_type("x/t")), nodes = list(
    tr_node("x/com", fn = function(data) data, description = "Com ajuda.",
            help = "## Descrição\n\nTexto longo.",
            inputs = list(data = "x/t"), outputs = list(out = "x/t")),
    tr_node("x/sem", fn = function(data) data, description = "Sem ajuda.",
            inputs = list(data = "x/t"), outputs = list(out = "x/t"))
  )), registry = reg)

  cat_nodes <- stats::setNames(tr_catalog(reg)$nodes, vapply(tr_catalog(reg)$nodes, function(n) n$id, ""))
  expect_match(cat_nodes[["x/com"]]$help, "Texto longo")
  expect_false("help" %in% names(cat_nodes[["x/sem"]]))
})

test_that("label da coleção viaja no catálogo", {
  cat <- tr_catalog(test_registry())
  expect_equal(cat$collections[[1]]$label, "Teste")
})

test_that("coleção sem label chega ao front com o id no lugar", {
  col <- tr_collection(id = "t", types = list(tr_type("t/num")))
  reg <- tr_registry(); tr_use(col, registry = reg)
  expect_equal(tr_catalog(reg)$collections[[1]]$label, "t")
})

test_that("ícone viaja no catálogo nas duas formas, e some quando ausente", {
  reg <- tr_registry()
  tr_use(tr_collection(
    id = "t", types = list(tr_type("t/num")),
    nodes = list(
      tr_node("t/com_set", fn = function() 1, description = "Tem ícone do conjunto.",
              outputs = list(out = "t/num"), icon = tr_icon("eraser")),
      tr_node("t/com_svg", fn = function() 1, description = "Tem ícone próprio.",
              outputs = list(out = "t/num"), icon = tr_icon(svg = "<path d='M3 3'/>")),
      tr_node("t/sem", fn = function() 1, description = "Não tem ícone.",
              outputs = list(out = "t/num"))
    )
  ), registry = reg)

  por_id <- function(cat, id) Filter(function(n) n$id == id, cat$nodes)[[1]]
  cat <- tr_catalog(reg)

  expect_equal(por_id(cat, "t/com_set")$icon, list(kind = "set", value = "eraser"))
  expect_equal(por_id(cat, "t/com_svg")$icon, list(kind = "svg", value = "<path d='M3 3'/>"))
  # AUSENTE, não NULL: o front testa presença de campo.
  expect_false("icon" %in% names(por_id(cat, "t/sem")))
})

test_that("ícone sobrevive à serialização JSON", {
  reg <- tr_registry()
  tr_use(tr_collection(
    id = "t", types = list(tr_type("t/num")),
    nodes = list(tr_node("t/a", fn = function() 1, description = "Com ícone.",
                         outputs = list(out = "t/num"), icon = tr_icon("eraser")))
  ), registry = reg)
  round <- jsonlite::fromJSON(tr_catalog_json(reg), simplifyVector = FALSE)
  expect_equal(round$nodes[[1]]$icon$kind, "set")
  expect_equal(round$nodes[[1]]$icon$value, "eraser")
})
