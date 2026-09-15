# Movidos de test-etl.R: test-pool.R e test-flow.R também precisam deles, e
# testthat carrega helper-*.R antes de qualquer test-*.R — é o único jeito de
# os arquivos de teste enxergarem o mesmo fixture sem duplicar.

skip_if_no_data <- function() {
  skip_if_not(dir.exists("../../collections/trama.data"), "coleção trama.data ausente")
  skip_if_not_installed("dplyr"); skip_if_not_installed("readr")
}

etl_registry <- function() {
  suppressMessages(pkgload::load_all("../../collections/trama.data", quiet = TRUE,
                                     export_all = FALSE, attach = FALSE))
  reg <- tr_registry(); tr_use("trama.data", registry = reg); reg
}

fixture_csv <- function() {
  p <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    regiao = c("sul", "sul", "norte", "norte", "sul", "norte"),
    produto = c("a", "b", "a", "b", "a", "a"),
    valor = c(10, 20, 30, 40, 5, 15),
    qtd = c(1, 2, 3, 4, 1, 2)
  ), p, row.names = FALSE)
  p
}

etl_doc <- function(reg, csv) {
  ops <- list(
    list(op = "add_node", type = "data/read_csv", id = "ler", params = list(path = csv)),
    list(op = "add_node", type = "data/filter", id = "filtrar",
         params = list(expr = "valor > 8")),
    list(op = "add_node", type = "data/mutate", id = "calc",
         params = list(name = "total", expr = "valor * qtd")),
    list(op = "add_node", type = "data/group_summarise", id = "resumo",
         params = list(by = "regiao", name = "total_regiao", expr = "sum(total)")),
    list(op = "add_node", type = "data/arrange", id = "ordenar",
         params = list(cols = "total_regiao", desc = TRUE)),
    list(op = "connect", from_node = "ler",     from_port = "out", to_node = "filtrar", to_port = "data"),
    list(op = "connect", from_node = "filtrar", from_port = "out", to_node = "calc",    to_port = "data"),
    list(op = "connect", from_node = "calc",    from_port = "out", to_node = "resumo",  to_port = "data"),
    list(op = "connect", from_node = "resumo",  from_port = "out", to_node = "ordenar", to_port = "data")
  )
  doc <- tr_doc()
  for (op in ops) doc <- tr_doc_apply(doc, op, reg)
  doc
}
