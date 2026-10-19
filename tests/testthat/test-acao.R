# Ação de card (`tr_type(acoes = )`, acao.R): roda fora da execução, sobre o
# valor gravado na chave que o card mostra.

acao_projeto <- function(acoes) {
  col <- tr_collection(
    id = "t", types = list(tr_type("t/num", acoes = acoes)),
    nodes = list(tr_node("t/k", fn = function(v) v, description = "Devolve o número escolhido.",
                         outputs = list(out = "t/num"), params = list(v = tr_param_num(1)))))
  reg <- tr_registry(); tr_use(col, registry = reg)
  s <- tr_store(tempfile())
  h <- tr_store_put(s, "chave1", 41, reg$types[["t/num"]])
  list(proj = list(registry = reg, store = s, root = tempdir()), chave = "chave1")
}

test_that("a ação recebe o valor do store e o ctx, e a resposta sai pelo responder", {
  visto <- NULL
  p <- acao_projeto(list(dobro = function(valor, ctx) { visto <<- ctx; list(resultado = valor * 2) }))
  res <- NULL
  .tr_acao_rodar(list(tipo = "t/num", chave = p$chave, acao = "dobro"), p$proj, tr_doc(), "main",
                 function(r) res <<- r, function(dir, nome) "x/")
  expect_identical(res, list(resultado = 82))
  expect_identical(visto$chave, "chave1"); expect_identical(visto$flow, "main")
  expect_true(is.function(visto$servir)); expect_true(is.function(visto$responder))
})

test_that("ação que devolve NULL responde depois, sozinha", {
  guardado <- NULL
  p <- acao_projeto(list(depois = function(valor, ctx) { guardado <<- ctx$responder; NULL }))
  res <- list()
  .tr_acao_rodar(list(tipo = "t/num", chave = p$chave, acao = "depois"), p$proj, tr_doc(), "main",
                 function(r) res[[length(res) + 1L]] <<- r, function(dir, nome) "x/")
  expect_length(res, 0L)
  guardado(list(ok = TRUE))
  expect_identical(res, list(list(ok = TRUE)))
})

test_that("tipo, ação ou chave que não existem são recusados com classe", {
  p <- acao_projeto(list(dobro = function(valor, ctx) list()))
  r <- function(req) .tr_acao_rodar(req, p$proj, tr_doc(), "main", function(x) NULL, function(d, n) "")
  expect_error(r(list(tipo = "t/nada", chave = p$chave, acao = "dobro")), class = "tr_error_bad_action")
  expect_error(r(list(tipo = "t/num", chave = p$chave, acao = "nada")), class = "tr_error_bad_action")
  expect_error(r(list(tipo = "t/num", chave = "outra", acao = "dobro")), class = "tr_error_bad_action")
  expect_error(r(list(tipo = "t/num", chave = "../../etc", acao = "dobro")), class = "tr_error_bad_action")
})

test_that("acoes mal declaradas são recusadas no tr_type", {
  expect_error(tr_type("t/x", acoes = list(function(v, ctx) NULL)), class = "tr_error_bad_type")
  expect_error(tr_type("t/x", acoes = list(a = 1)), class = "tr_error_bad_type")
})

test_that("servir recusa nome que não serve de prefixo de URL", {
  expect_error(.tr_acao_servir(tempdir(), "../x"), class = "tr_error_bad_action")
})
