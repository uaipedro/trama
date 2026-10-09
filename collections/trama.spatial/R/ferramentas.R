# Ferramentas de fora da base do R que cada bloco usou numa corrida.
#
# O relatório exportado cita as ferramentas (pacote::função) de cada bloco. Quando
# o bloco usa ferramentas que dependem do ramo (o modelo ajustado, a krigagem),
# ele registra no resultado SÓ as do ramo que correu, no atributo
# `trama_ferramentas`. Sem o atributo, o relatório cai no que o bloco declara nas
# `tr_ref` de implementação.
.tr_spatial_ferramentas <- function(x, ferramentas) {
  attr(x, "trama_ferramentas") <- unique(as.character(ferramentas))
  x
}

