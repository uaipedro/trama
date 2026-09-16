# Nó que dorme: existe só para testar cancelamento no pool. `Sys.sleep` é
# interrompível, o que é o melhor caso; C++ de terra não seria.
#
# E uma REGIÃO DE FLUXO mínima (fonte, membro elevado que morre num ponto
# escolhido, colapso). Ela mora aqui, e não na coleção `d/*` de
# `test-stream-driver.R`, por um motivo estrutural: coleção definida em
# `globalenv()` não tem `package`, e `tr_executor_pool()` a RECUSA por desenho —
# um daemon não teria o que carregar. Sem um pacote de verdade, tudo o que
# depende de atravessar a fronteira de processo (o `ctx_extra` da Tarefa 5.4,
# entre outros) ficaria "validado só por leitura".
#
# O ponto em que o membro morre é PARAM, e não estado do processo de teste como
# em `d/puro_morre`: o `fn` roda em outro processo, onde nenhum ambiente do teste
# existe. Custo assumido: o param entra na chave da região, então mudar o ponto
# de morte muda a chave — o que é correto (é conteúdo do grafo) e basta lembrar
# ao escrever um teste de retomada com ele.
trama_collection <- function() trama::tr_collection(
  id = "slow", version = "0.0.1",
  types = list(trama::tr_type("slow/x")),
  nodes = list(
    trama::tr_node("slow/sleep", fn = function(secs, .ctx) {
      for (i in seq_len(10)) { Sys.sleep(secs / 10); .ctx$progress(i / 10) }
      secs
    }, description = "Dorme pelos segundos pedidos, publicando progresso.",
       outputs = list(out = "slow/x"), params = list(secs = trama::tr_param_num(2))),
    trama::tr_node("slow/pontos", fn = function(n) as.list(seq_len(n)),
      outputs = list(out = trama::tr_port("slow/x", stream = TRUE)),
      params = list(n = trama::tr_param_int(10L)),
      description = "Emite n pontos: 1..n."),
    # `stop()` cru, e não `rlang::abort()` classificado: a fixture depende só de
    # `trama`, e o que o teste mede é o CHECKPOINT no disco, não a classe do erro.
    trama::tr_node("slow/morre", fn = function(x, em) {
      if (isTRUE(as.numeric(x) == em)) stop("worker morto no ponto ", em)
      x
    }, inputs = list(x = "slow/x"), outputs = list(out = "slow/x"),
       params = list(em = trama::tr_param_num(0)),
       description = "Passa o ponto adiante, e morre no ponto escolhido."),
    trama::tr_node("slow/junta", fn = function(x) if (length(x)) unlist(x) else numeric(),
      inputs = list(x = trama::tr_port("slow/x", stream = TRUE)),
      outputs = list(out = "slow/x"),
      description = "Junta os pontos num histórico.")))
