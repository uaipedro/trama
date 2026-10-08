test_that("raio-x segue funções internas e acha a chamada externa com a linha", {
  amb <- local({
    .ajudante <- function(x) stats::median(x[!is.na(x)])
    calcular <- function(dados) {
      y <- .ajudante(dados$y)
      list(valor = y, n = length(dados$y))
    }
    environment()
  })
  no <- tr_node("teste/mediana", fn = amb$calcular, description = "Mediana.",
                inputs = list(dados = "t/num"))
  rx <- tr_node_raiox(no)
  externas <- vapply(rx$chamadas, function(c) paste0(c$pacote, "::", c$funcao), "")
  expect_true("stats::median" %in% externas)
  expect_false(any(startsWith(externas, "base::")))
  med <- rx$chamadas[[which(externas == "stats::median")]]
  expect_equal(med$dentro, ".ajudante")
  expect_match(med$codigo, "^stats::median\\(")
})

test_that("raio-x de um bloco sem chamadas externas devolve lista vazia", {
  no <- tr_node("teste/soma", fn = function(dados) sum(dados$y), description = "Soma.",
                inputs = list(dados = "t/num"))
  expect_length(tr_node_raiox(no)$chamadas, 0L)
})
