# Mesma doutrina das coleções irmãs: a maior parte dos nós se testa pelo NÍVEL
# 1 — `tr_series_diff(serie)` é chamada de função R comum, sem registro e sem
# motor. Se o nó só se testa através do grafo, ele deixou de ser função R comum.

# Mensal, com tendência e sazonalidade de verdade, e curta o bastante para os
# testes rodarem rápido. `AirPassengers` é o caso de livro, e é também o
# default do `series/example`.
serie_mensal <- function() datasets::AirPassengers

serie_anual <- function() datasets::Nile

tabela_mensal <- function() {
  tibble::tibble(
    mes   = seq(as.Date("2020-01-01"), by = "month", length.out = 36),
    vendas = as.numeric(100 + 1:36 + 10 * sin(2 * pi * (1:36) / 12))
  )
}

# A `series` NÃO carrega sozinha: as portas são `data/table` (da `data`) e
# `view/plot` (da `view`). A ordem aqui é a mesma que o `trama.json` de um
# projeto precisa ter.
series_registry <- function() {
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
