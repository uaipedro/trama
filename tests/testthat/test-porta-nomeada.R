# `tr_port(named = TRUE)`: a porta múltipla chega nomeada pelo nó de origem,
# e o nome entra na chave de cache.

nomeada_registry <- function() {
  col <- tr_collection(
    id = "t", types = list(tr_type("t/num")),
    nodes = list(
      tr_node("t/k", fn = function(v) v, description = "Devolve o número escolhido.",
              outputs = list(out = "t/num"), params = list(v = tr_param_num(1))),
      tr_node("t/nomes", fn = function(xs) paste(names(xs), unlist(xs), sep = "=", collapse = ","),
              description = "Lista as entradas pelo nome do nó de origem.",
              inputs = list(xs = tr_port("t/num", multiple = TRUE, named = TRUE)),
              outputs = list(out = "t/num"))))
  reg <- tr_registry(); tr_use(col, registry = reg); reg
}

test_that("porta named entrega a lista nomeada pelos nós de origem", {
  reg <- nomeada_registry()
  f <- tr_flow(reg) |> tr_add("a", "t/k", v = 1) |> tr_add("b", "t/k", v = 2) |>
    tr_add("j", "t/nomes", from = c("a", "b"))
  expect_identical(tr_value(tr_flow_doc(f), "j", registry = reg, store = tr_store(tempfile())), "a=1,b=2")
  expect_true(tr_catalog(reg)$nodes[[2]]$inputs[[1]]$named)
})

test_that("trocar a origem por outro nó de mesmo valor muda a chave", {
  reg <- nomeada_registry()
  f1 <- tr_flow(reg) |> tr_add("a", "t/k", v = 1) |> tr_add("j", "t/nomes", from = "a")
  f2 <- tr_flow(reg) |> tr_add("z", "t/k", v = 1) |> tr_add("j", "t/nomes", from = "z")
  s <- tr_store(tempfile())
  expect_identical(tr_value(tr_flow_doc(f1), "j", registry = reg, store = s), "a=1")
  expect_identical(tr_value(tr_flow_doc(f2), "j", registry = reg, store = s), "z=1")
})

test_that("named sem multiple é recusado", {
  expect_error(tr_port("t/num", named = TRUE), class = "tr_error_bad_port")
})

test_that("nomes repetidos ganham a saída", {
  expect_identical(.tr_port_nomes(list(list(node = "a", port = "x"), list(node = "a", port = "y"),
                                       list(node = "b", port = "out"))),
                   c("a.x", "a.y", "b"))
})
