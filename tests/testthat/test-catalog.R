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

test_that("categoria com papel manda o papel e não a cor", {
  k <- tr_category("fit", "Ajustar", role = "ajuste")
  expect_equal(k$role, "ajuste")
  expect_null(k$color)
  expect_equal(tr_category("x", "X", "#123456")$color, "#123456")
  expect_error(tr_category("x", "X", role = "modelagem"), class = "tr_error_bad_role")
})

test_that("nó pode declarar o próprio papel, validado como o da categoria", {
  n <- tr_node("t/prever", function() 1, description = "prevê", role = "leitura")
  expect_equal(n$role, "leitura")
  expect_null(tr_node("t/x", function() 1, description = "x")$role)
  expect_error(tr_node("t/y", function() 1, description = "y", role = "analise"),
               class = "tr_error_bad_role")
})

test_that("transições declaradas pela coleção viajam no catálogo", {
  col <- tr_collection(
    id = "t", types = list(tr_type("t/num")),
    nodes = list(tr_node("t/um", fn = function() 1, description = "Um.",
                         outputs = list(out = "t/num"))),
    transitions = data.frame(from = c("x/ler", "t/um"), to = c("t/um", "t/um2"), n = c(3L, 1L))
  )
  reg <- tr_registry(); tr_use(col, registry = reg)
  tr <- tr_catalog(reg)$transitions
  expect_length(tr, 2)
  expect_equal(tr[[1]], list(from = "x/ler", to = "t/um", n = 3L))
})

test_that("transição com `to` fora do namespace aborta", {
  expect_error(
    tr_collection("t", transitions = data.frame(from = "t/a", to = "x/b", n = 1L)),
    class = "tr_error_foreign_id")
})

test_that("transição com n não positivo aborta", {
  expect_error(
    tr_collection("t", transitions = data.frame(from = "x/a", to = "t/b", n = 0L)),
    class = "tr_error_bad_transition")
})

test_that("sem transições, o campo some do catálogo", {
  expect_false("transitions" %in% names(tr_catalog(test_registry())))
})

test_that("tr_when anexa condições e recusa o malformado", {
  p <- tr_when(tr_param_num(0.05), metodo = c("holm", "bonferroni"), ajustar = TRUE)
  expect_equal(p$when, list(metodo = c("holm", "bonferroni"), ajustar = TRUE))
  expect_error(tr_when(0.05, metodo = "holm"), class = "tr_error_bad_when")
  expect_error(tr_when(tr_param_num(1)), class = "tr_error_bad_when")
  expect_error(tr_when(tr_param_num(1), "holm"), class = "tr_error_bad_when")
  expect_error(tr_when(tr_param_num(1), metodo = list("a")), class = "tr_error_bad_when")
})

test_that("when sai no catálogo com cada valor como array", {
  n <- tr_node("t/w", fn = function(metodo, alfa) alfa,
               description = "Testa params condicionais.",
               params = list(metodo = tr_param_enum("holm", c("holm", "none")),
                             alfa = tr_when(tr_param_num(0.05), metodo = "holm")))
  col <- tr_collection(id = "t", nodes = list(n))
  reg <- tr_registry(); tr_use(col, registry = reg)
  j <- as.character(tr_catalog_json(reg))
  expect_match(j, '"when":\\{"metodo":\\["holm"\\]\\}')
})

test_that("when que cita param inexistente é recusado na declaração", {
  expect_error(
    tr_node("t/w", fn = function(alfa) alfa, description = "x",
            params = list(alfa = tr_when(tr_param_num(0.05), metodo = "holm"))),
    class = "tr_error_bad_when")
  expect_error(
    tr_node("t/w", fn = function(alfa) alfa, description = "x",
            params = list(alfa = tr_when(tr_param_num(0.05), alfa = 1))),
    class = "tr_error_bad_when")
})

test_that("tr_param_col anota role/multi/from e recusa o malformado", {
  p <- tr_param_col(role = "numerica", multi = TRUE, from = "dados")
  expect_equal(p$kind, "cols")
  expect_equal(p$role, "numerica"); expect_true(p$multi); expect_equal(p$from, "dados")
  expect_equal(tr_param_col()$role, "qualquer")
  expect_false(tr_param_col()$multi)
  expect_error(tr_param_col(role = "texto"), class = "tr_error_bad_param")
  expect_error(tr_param_col(role = c("numerica", "tempo")), class = "tr_error_bad_param")
  expect_error(tr_param_col(from = ""), class = "tr_error_bad_param")
})

test_that("from de tr_param_col precisa ser entrada do nó", {
  expect_error(
    tr_node("t/c", fn = function(x, col) x, description = "x",
            inputs = list(x = "t/box"), params = list(col = tr_param_col(from = "y"))),
    class = "tr_error_bad_param")
  expect_s3_class(
    tr_node("t/c", fn = function(x, col) x, description = "x",
            inputs = list(x = "t/box"), params = list(col = tr_param_col(from = "x"))),
    "tr_node")
})

test_that("param de coluna leva role/multi/from ao catálogo; ausentes somem", {
  ty <- tr_type("t/df", version = 1L)
  n <- tr_node("t/c", fn = function(x, a, b) x, description = "Testa param de coluna.",
               inputs = list(x = "t/df"), outputs = list(out = "t/df"),
               params = list(a = tr_param_col(role = "numerica", from = "x", example = "peso"),
                             b = tr_param_col(multi = TRUE)))
  reg <- tr_registry(); tr_use(tr_collection(id = "t", types = list(ty), nodes = list(n)), registry = reg)
  cat <- jsonlite::fromJSON(as.character(tr_catalog_json(reg)), simplifyVector = FALSE)
  ps <- cat$nodes[[1]]$params
  expect_equal(ps[[1]]$role, "numerica"); expect_false(ps[[1]]$multi); expect_equal(ps[[1]]$from, "x")
  expect_equal(ps[[1]]$example, "peso")
  expect_equal(ps[[2]]$role, "qualquer"); expect_true(ps[[2]]$multi)
  expect_false("from" %in% names(ps[[2]])); expect_false("example" %in% names(ps[[2]]))
})
