# Mesma doutrina da coleção `data`: a maior parte dos nós se testa pelo NÍVEL 1
# — `tr_points(df, "x", "y")` é chamada de função R comum, sem registro e sem
# motor. Se o nó só se testa através do grafo, ele deixou de ser função R comum.

df_exemplo <- function() {
  tibble::tibble(
    regiao  = c("sul", "sul", "norte", "norte", "sul", "norte"),
    produto = c("a", "b", "a", "b", "a", "a"),
    valor   = c(10, 20, 30, 40, 5, 15),
    qtd     = c(1L, 2L, 3L, 4L, 1L, 2L)
  )
}

# A coleção `view` NÃO carrega sozinha: as portas de entrada são `data/table`,
# que pertence à `data`. A ordem aqui é a mesma que o `trama.json` de um projeto
# precisa ter.
view_registry <- function() {
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  trama::tr_use(trama_collection(), registry = reg)
  reg
}
