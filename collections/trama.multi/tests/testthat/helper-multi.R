# Mesma doutrina das coleções irmãs: a maior parte dos nós se testa pelo NÍVEL
# 1 — `tr_multi_pca(dados)` é chamada de função R comum, sem registro e sem
# motor.

iris_t <- function() tr_multi_example("iris")

# A `multi` NÃO carrega sozinha: as portas são `data/table` (da `data`) e
# `view/plot` (da `view`). A ordem aqui é a mesma que o `trama.json` precisa.
multi_registry <- function() {
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  trama::tr_use("trama.view", registry = reg)
  trama::tr_use(trama_collection(), registry = reg)
  reg
}

ctx_tmp <- function() {
  dir <- tempfile(); dir.create(dir)
  list(file = function(e) file.path(dir, paste0("pv.", e)))
}

rodar <- function(flow, no, port = NULL) {
  s <- trama::tr_store(tempfile())
  trama::tr_value(trama::tr_flow_doc(flow), no, registry = flow$registry, store = s, port = port)
}

# Compara duas matrizes de cargas/vetores até o SINAL de cada coluna, que é
# arbitrário em PCA, fatorial e discriminante: o mesmo fator com sinal trocado
# é a mesma solução.
expect_equal_ate_sinal <- function(a, b, tolerance = 1e-6, ...) {
  a <- unname(as.matrix(a)); b <- unname(as.matrix(b))
  expect_equal(dim(a), dim(b))
  for (j in seq_len(ncol(a))) {
    s <- sign(sum(a[, j] * b[, j])); if (s == 0) s <- 1
    expect_equal(a[, j] * s, b[, j], tolerance = tolerance, ...)
  }
}
