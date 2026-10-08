# Mesma doutrina das irmãs: a maior parte dos nós se testa pelo NÍVEL 1 —
# chamada de função R comum, sem registro e sem motor.

# A `spatial` NÃO carrega sozinha: as portas são `data/table` (da `data`) e
# `view/plot` (da `view`). A ordem aqui é a mesma que o `trama.json` precisa.
spatial_registry <- function() {
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
