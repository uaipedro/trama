test_that("id de tipo e de nó precisa ser qualificado por coleção", {
  expect_error(tr_type("num"), class = "tr_error_bad_id")
  expect_error(tr_type("T/Num"), class = "tr_error_bad_id")
  expect_error(tr_node("add", fn = function() NULL, description = "x"), class = "tr_error_bad_id")
  expect_s3_class(tr_type("t/num"), "tr_type")
})

test_that("store e restore andam juntos", {
  expect_error(tr_type("t/x", store = function(x, path) NULL), class = "tr_error_bad_type")
  expect_s3_class(tr_type("t/x", store = function(x, path) NULL, restore = function(path) NULL), "tr_type")
})

test_that("argumento de fn sem input/param correspondente é erro alto e cedo", {
  expect_error(
    tr_node("t/n", fn = function(a, b) a, description = "x", inputs = list(a = "t/num")),
    class = "tr_error_unknown_fn_arg"
  )
  # .seed e .ctx são reservados e sempre permitidos
  expect_s3_class(
    tr_node("t/n", fn = function(a, .seed, .ctx) a, description = "x", inputs = list(a = "t/num")),
    "tr_node"
  )
})

test_that(".seed no fn (ou no step) infere stochastic, mesmo sem marcar", {
  # Achado da pauta pós-região de fluxo: `.seed` sem `stochastic = TRUE`
  # deixava a seed mudar o resultado sem entrar na chave de cache.
  n <- tr_node("t/n", fn = function(a, .seed) a, description = "x", inputs = list(a = "t/num"))
  expect_true(n$stochastic)

  n2 <- tr_node("t/n", fn = function(a) a, description = "x", inputs = list(a = "t/num"))
  expect_false(n2$stochastic)

  n3 <- tr_node("t/n", fn = function(fluxo) fluxo,
                inputs = list(fluxo = tr_port("t/num", stream = TRUE)),
                outputs = list(out = tr_port("t/num", stream = TRUE)),
                description = "x",
                init = function() NULL, step = function(state, fluxo, .seed) list(state = state, out = fluxo))
  expect_true(n3$stochastic)
})

test_that("nome não pode ser input e param ao mesmo tempo", {
  expect_error(
    tr_node("t/n", fn = function(a) a, description = "x", inputs = list(a = "t/num"),
            params = list(a = tr_param_num(1))),
    class = "tr_error_name_collision"
  )
})

test_that("param exige default e nó impuro exige fingerprint", {
  expect_error(tr_param("number"), class = "tr_error_param_no_default")
  expect_error(
    tr_node("t/n", fn = function() NULL, description = "x", pure = FALSE),
    class = "tr_error_missing_fingerprint"
  )
  expect_s3_class(
    tr_node("t/n", fn = function() NULL, description = "x", pure = FALSE, fingerprint = function(params) "x"),
    "tr_node"
  )
})

test_that("coleção não pode declarar id fora do próprio namespace", {
  expect_error(
    tr_collection(id = "t", types = list(tr_type("outra/x"))),
    class = "tr_error_foreign_id"
  )
})

test_that("porta com tipo desconhecido falha ao carregar a coleção", {
  col <- tr_collection(
    id = "t", types = list(tr_type("t/num")),
    nodes = list(tr_node("t/n", fn = function(x) x, description = "x", inputs = list(x = "t/inexistente")))
  )
  expect_error(tr_use(col, registry = tr_registry()), class = "tr_error_unknown_type")
})

test_that("registros são isolados entre si", {
  a <- test_registry()
  b <- tr_registry()
  expect_length(a$nodes, 3)
  expect_length(b$nodes, 0)
  expect_error(tr_get_node("t/add", registry = b), class = "tr_error_unknown_node")
})

test_that("carregar a mesma coleção duas vezes é erro", {
  reg <- test_registry()
  expect_error(tr_use(test_collection(), registry = reg), class = "tr_error_duplicate_collection")
})

test_that("nó sem description é recusado, e 'help' viaja na spec", {
  f <- function(data) data
  expect_error(tr_node("x/a", fn = f, inputs = list(data = "x/t"), outputs = list(out = "x/t")),
               class = "tr_error_missing_description")
  expect_error(tr_node("x/a", fn = f, description = "   ",
                       inputs = list(data = "x/t"), outputs = list(out = "x/t")),
               class = "tr_error_missing_description")
  # `trimws(NA_character_)` devolve a string "NA", que passa por `nzchar()`:
  # sem checar NA explicitamente o nó nasce mudo e o catálogo derruba o campo.
  expect_error(tr_node("x/a", fn = f, description = NA_character_,
                       inputs = list(data = "x/t"), outputs = list(out = "x/t")),
               class = "tr_error_missing_description")

  # `help` vira markdown no painel lateral: o contrato é texto, não número.
  expect_error(tr_node("x/a", fn = f, description = "Faz nada.", help = 42,
                       inputs = list(data = "x/t"), outputs = list(out = "x/t")),
               class = "tr_error_bad_help")
  expect_error(tr_node("x/a", fn = f, description = "Faz nada.", help = NA_character_,
                       inputs = list(data = "x/t"), outputs = list(out = "x/t")),
               class = "tr_error_bad_help")
  expect_error(tr_node("x/a", fn = f, description = "Faz nada.", help = c("a", "b"),
                       inputs = list(data = "x/t"), outputs = list(out = "x/t")),
               class = "tr_error_bad_help")

  # `icon` tem que vir de tr_icon(): uma lista com a forma certa mas sem a
  # classe passaria direto pro catálogo e só quebraria no front, longe daqui.
  expect_error(tr_node("x/a", fn = f, description = "Faz nada.",
                       icon = list(kind = "set", value = "eraser"),
                       inputs = list(data = "x/t"), outputs = list(out = "x/t")),
               class = "tr_error_bad_icon")
  expect_error(tr_node("x/a", fn = f, description = "Faz nada.", icon = "eraser",
                       inputs = list(data = "x/t"), outputs = list(out = "x/t")),
               class = "tr_error_bad_icon")

  n <- tr_node("x/a", fn = f, description = "Faz nada.", help = "## Descrição\n\nNada mesmo.",
               inputs = list(data = "x/t"), outputs = list(out = "x/t"))
  expect_equal(n$description, "Faz nada.")
  expect_match(n$help, "Nada mesmo")
})

test_that("migrações declaradas acumulam no registro, inclusive vindas de outra coleção", {
  reg <- test_registry()
  tr_use(tr_collection(id = "u", migrations = list(
    nodes  = list("t/velho" = "u/novo", "outra/roc" = "u/roc"),
    params = list("u/novo" = list(alfa = list(to = "confianca", value = function(v) 1 - v))),
    ports  = list("u/novo" = list(data = "dados"))
  )), registry = reg)
  expect_equal(reg$migrations$nodes[["t/velho"]], "u/novo")
  expect_equal(reg$migrations$nodes[["outra/roc"]], "u/roc")
  expect_equal(reg$migrations$params[["u/novo"]]$alfa$to, "confianca")
  expect_equal(reg$migrations$ports[["u/novo"]]$data, "dados")
})

test_that("migração com destino fora do namespace, malformada ou conflitante é recusada", {
  expect_error(tr_collection(id = "u", migrations = list(nodes = list("u/a" = "v/b"))),
               class = "tr_error_foreign_id")
  expect_error(tr_collection(id = "u", migrations = list(params = list("v/b" = list()))),
               class = "tr_error_foreign_id")
  expect_error(tr_collection(id = "u", migrations = list(nodes = list("x/a" = "u/b", "x/a" = "u/c"))),
               class = "tr_error_bad_migration")
  expect_error(tr_collection(id = "u", migrations = list(params = list("u/b" = list(p = "q")))),
               class = "tr_error_bad_migration")
  # Converter no lugar sem `when` não teria como pular doc já migrado.
  expect_error(tr_collection(id = "u", migrations = list(params = list(
    "u/b" = list(p = list(to = "p", value = as.numeric))))), class = "tr_error_bad_migration")
  expect_error(tr_collection(id = "u", migrations = list(params = list(
    "u/b" = list(p = list(to = "q", when = TRUE))))), class = "tr_error_bad_migration")
  expect_s3_class(tr_collection(id = "u", migrations = list(params = list(
    "u/b" = list(p = list(to = "p", when = is.character, value = as.numeric))))), "tr_collection")
  reg <- tr_registry()
  tr_use(tr_collection(id = "u", migrations = list(nodes = list("x/a" = "u/b"))), registry = reg)
  expect_error(tr_use(tr_collection(id = "v", migrations = list(nodes = list("x/a" = "v/b"))),
                      registry = reg), class = "tr_error_bad_migration")
  # O erro não deixa a segunda coleção meio carregada.
  expect_null(reg$collections$v)
})
