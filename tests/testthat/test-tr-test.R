# O registro genérico de teste: a regra de decisão tem de ser a MESMA do
# `teste.js::venceu`, senão selo e conclusão do card divergem.

test_that("decide pelo p-valor quando há, pelo crítico de 5% na cauda quando não", {
  sim <- "rejeita"; nao <- "não"
  expect_equal(tr_test("a", "h", 2, p_valor = 0.01, conclusao_sim = sim, conclusao_nao = nao)$conclusao, sim)
  expect_equal(tr_test("a", "h", 2, p_valor = 0.2, conclusao_sim = sim, conclusao_nao = nao)$decisao_5,
               "não rejeita H0")
  cv <- c(`10%` = -2.57, `5%` = -2.88, `1%` = -3.46)
  expect_equal(tr_test("a", "h", -3, criticos = cv, sentido = "menor",
                       conclusao_sim = sim, conclusao_nao = nao)$decisao_5, "rejeita H0")
  expect_equal(tr_test("a", "h", -3, criticos = cv, sentido = "maior",
                       conclusao_sim = sim, conclusao_nao = nao)$decisao_5, "não rejeita H0")
  expect_error(tr_test("a", "h", 1, conclusao_sim = sim, conclusao_nao = nao), class = "tr_error_bad_test")
  expect_error(tr_test("a", "h", 1, p_valor = 0.1, sentido = "x", conclusao_sim = sim, conclusao_nao = nao),
               class = "tr_error_bad_test")
})

test_that("tipo: store recusa o que não é teste, round-trip e preview no card", {
  ty <- tr_test_type("x/test")
  p <- tempfile(fileext = ".rds")
  expect_error(ty$store(list(teste = "a"), p), class = "tr_error_not_a_test")
  t <- tr_test("a", "h", 2, p_valor = 0.01, gl = "3", conclusao_sim = "s", conclusao_nao = "n",
               efeito = list(rotulo = "d", valor = 1, li = NA_real_, ls = 2), classe = "x_test")
  ty$store(t, p)
  expect_identical(ty$restore(p), t)
  pv <- ty$preview(t, list())
  expect_equal(pv$renderer, "trama/test")
  expect_null(pv$data$efeito$li)
})

test_that("tabela: colunas fixas, extra só quando existe", {
  fixas <- c("teste", "h0", "rotulo_estat", "estatistica", "gl", "p_valor", "significancia",
             "valor_critico_5", "decisao_5", "conclusao", "efeito", "efeito_valor",
             "efeito_li_95", "efeito_ls_95", "nota", "fonte")
  a <- tr_test_table(tr_test("a", "h", 2, p_valor = 0.004, conclusao_sim = "s", conclusao_nao = "n"))
  b <- tr_test_table(tr_test("b", "h", -3, criticos = c(`5%` = -2.9), conclusao_sim = "s",
                             conclusao_nao = "n", extra = list(k = 4)))
  expect_equal(names(a), fixas)
  expect_equal(names(b), c(fixas, "extra_k"))
  expect_equal(a$significancia, "**")
  expect_true(is.na(b$p_valor)); expect_equal(b$valor_critico_5, -2.9)
})

test_that("extra com nome de contraste ou de coluna fixa vira coluna prefixada, intacta", {
  t <- tr_test("a", "h", 2, p_valor = 0.01, conclusao_sim = "s", conclusao_nao = "n",
               extra = list(`A - B` = 1.5, nota = 2))
  tb <- tr_test_table(t)
  expect_true(all(c("extra_A - B", "extra_nota", "nota") %in% names(tb)))
  expect_false(anyDuplicated(names(tb)) > 0)
  expect_equal(tb[["extra_A - B"]], 1.5)
  expect_no_error(q <- rbind(tb, tb))
})

test_that("efeito: IC é de 95%; nível diferente é erro", {
  ok <- tr_test("a", "h", 2, p_valor = 0.01, conclusao_sim = "s", conclusao_nao = "n",
                efeito = list(rotulo = "d", valor = 1, li = 0.5, ls = 1.5, nivel = 0.95))
  expect_equal(tr_test_table(ok)$efeito_li_95, 0.5)
  expect_error(tr_test("a", "h", 2, p_valor = 0.01, conclusao_sim = "s", conclusao_nao = "n",
                       efeito = list(rotulo = "d", valor = 1, li = 0, ls = 2, nivel = 0.9)),
               class = "tr_error_bad_test")
})

test_that("gl sai num formato só", {
  g <- function(gl) tr_test("a", "h", 2, p_valor = 0.1, gl = gl, conclusao_sim = "s",
                            conclusao_nao = "n")$gl
  expect_equal(g(20), "20"); expect_equal(g(20L), "20"); expect_equal(g(27.345), "27,3")
  expect_equal(g(c(2, 27)), "2; 27"); expect_equal(g("2; 27"), "2; 27"); expect_true(is.na(g(NA)))
})
