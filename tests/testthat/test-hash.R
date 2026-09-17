test_that("fingerprint enxerga helper chamado pela função — o furo do insumo", {
  # Reproduz o bug real de insumo/nodes/viewer.R: um helper definido no nível
  # do arquivo, chamado de dentro do fn. `hash(body(fn))` não muda quando o
  # helper muda, e o cache serve resultado velho em silêncio.
  # `local()` reproduz o que um namespace de coleção é: fn e helper no MESMO
  # ambiente, que é a situação real.
  f1 <- local({ palette <- function() c("a", "b"); function(x) palette()[x] })
  f2 <- local({ palette <- function() c("z", "w"); function(x) palette()[x] })

  expect_identical(rlang::hash(body(f1)), rlang::hash(body(f2)))  # o furo
  expect_false(identical(.tr_fn_fingerprint(f1), .tr_fn_fingerprint(f2)))
})

test_that("fingerprint também enxerga CONSTANTE do nível da coleção", {
  # A outra metade do furo: insumo/nodes/viewer.R tem tanto um helper função
  # quanto constantes de cor no nível do arquivo, usadas dentro do fn.
  g1 <- local({ COR <- c(1, 0, 0); function(x) x * COR })
  g2 <- local({ COR <- c(0, 1, 0); function(x) x * COR })
  expect_identical(rlang::hash(body(g1)), rlang::hash(body(g2)))
  expect_false(identical(.tr_fn_fingerprint(g1), .tr_fn_fingerprint(g2)))
})

test_that("fingerprint é estável quando nada muda e não entra em pacote externo", {
  f <- local({ helper <- function(x) x + 1; function(x) helper(x) + mean(x) })
  expect_identical(.tr_fn_fingerprint(f), .tr_fn_fingerprint(f))
  expect_type(.tr_fn_fingerprint(f), "character")
})

test_that("params efetivos misturam default do registro com o documento", {
  reg <- store_registry()
  spec <- tr_get_node("t/inc", reg)
  expect_equal(.tr_effective_params(spec, list(params = list()))$by, 1)
  expect_equal(.tr_effective_params(spec, list(params = list(by = 9)))$by, 9)
})

test_that(".seed sem stochastic explícito ainda muda a chave quando a seed muda", {
  # Achado da pauta pós-região de fluxo, repro exato: um nó que declara
  # `.seed` no `fn` mas deixa `stochastic` no default (FALSE) servia o mesmo
  # handle pra sempre depois de `tr_set_seed()`, porque a seed nunca entrava
  # na chave. Com a inferência em `tr_node()`, `tr_set_seed()` (op
  # `set_seed`) tem que mudar a chave.
  reg <- tr_registry()
  tr_use(tr_collection(
    id = "t", types = list(tr_type("t/num")),
    nodes = list(tr_node("t/sorteia", fn = function(.seed) .seed,
                         description = "Sorteia algo a partir da seed.",
                         outputs = list(out = "t/num")))
  ), registry = reg)

  doc <- build(reg, list(list(op = "add_node", type = "t/sorteia", id = "a")))
  key1 <- tr_plan(doc, registry = reg)$keys[["a"]]
  doc2 <- tr_doc_apply(doc, list(op = "set_seed", node = "a", value = doc$nodes[["a"]]$seed + 1L), reg)
  key2 <- tr_plan(doc2, registry = reg)$keys[["a"]]
  expect_false(identical(key1, key2))
})
