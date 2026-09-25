# O card de tabela da coleção data, usado pelo card do modelo (contrato.R).
#
# A ml não declara tipo: desde a Fase 4 os ajustes viajam como `models/fit`, o
# tipo da `trama.models`, e o card é o `tr_models_card()` da classe `tr_ml_fit`.

.tr_ml_table_preview <- function(x) {
  tipos <- trama.data::trama_collection()$types
  tabela <- Filter(function(tipo) identical(tipo$id, "data/table"), tipos)[[1L]]
  tabela$preview(x, NULL)
}
