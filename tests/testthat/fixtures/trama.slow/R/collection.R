# Nó que dorme: existe só para testar cancelamento no pool. `Sys.sleep` é
# interrompível, o que é o melhor caso; C++ de terra não seria.
trama_collection <- function() trama::tr_collection(
  id = "slow", version = "0.0.1",
  types = list(trama::tr_type("slow/x")),
  nodes = list(trama::tr_node("slow/sleep", fn = function(secs, .ctx) {
    for (i in seq_len(10)) { Sys.sleep(secs / 10); .ctx$progress(i / 10) }
    secs
  }, description = "Dorme pelos segundos pedidos, publicando progresso.",
     outputs = list(out = "slow/x"), params = list(secs = trama::tr_param_num(2)))))
