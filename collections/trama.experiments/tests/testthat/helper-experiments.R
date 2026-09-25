# Mesma doutrina das irmãs: as estruturas se testam pelo NÍVEL 1
# (`tr_experiments_design(...)`, sem motor); o motor só no que depende dele.

ds <- function(...) tr_experiments_design(...)

# A ordem é a que o `trama.json` precisa: data, view, models, experiments.
experiments_registry <- function() {
  reg <- trama::tr_registry()
  trama::tr_use("trama.data", registry = reg)
  trama::tr_use("trama.view", registry = reg)
  trama::tr_use("trama.models", registry = reg)
  trama::tr_use(trama_collection(), registry = reg)
  reg
}

rodar <- function(flow, no, port = NULL) {
  s <- trama::tr_store(tempfile())
  trama::tr_value(trama::tr_flow_doc(flow), no, registry = flow$registry, store = s, port = port)
}

# Contagem de cada tratamento dentro de cada nível de `por`.
contagem <- function(u, trat, por) table(u[[por]], u[[trat]])
