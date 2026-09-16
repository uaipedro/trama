# Coleção mínima de teste — o núcleo é validado SEM nenhuma coleção real.
# É a guarda contra acoplamento: se algum teste do núcleo precisar de `terra`,
# de raster ou de tidyverse, alguma suposição de domínio vazou.

test_collection <- function() {
  num <- tr_type("t/num", label = "Número", color = "#111111",
                 preview = function(x, ctx) tr_preview("t/num", data = list(value = x)))
  txt <- tr_type("t/txt", label = "Texto")

  tr_collection(
    id = "t", version = "1.0.0", label = "Teste",
    categories = list(tr_category("basic", "Básico")),
    types = list(num, txt),
    nodes = list(
      tr_node("t/const", fn = function(value) value, category = "basic",
              description = "Devolve o número escolhido.",
              outputs = list(out = "t/num"),
              params = list(value = tr_param_num(1))),
      tr_node("t/add", fn = function(a, b, k) a + b + k, category = "basic",
              description = "Soma duas entradas e uma constante.",
              inputs = list(a = "t/num", b = "t/num"),
              outputs = list(out = "t/num"),
              params = list(k = tr_param_num(0))),
      tr_node("t/show", fn = function(x) invisible(x), category = "basic",
              description = "Mostra o texto recebido.",
              inputs = list(x = "t/txt"))
    ),
    adapters = list(tr_adapter("t/num", "t/txt", as.character))
  )
}

test_registry <- function() {
  reg <- tr_registry()
  tr_use(test_collection(), registry = reg)
  reg
}

# Documento novo + registro de teste: o par que quase todo teste de op abre.
# Mora aqui, e não no topo de um test-*.R, porque mais de um arquivo precisa.
mk <- function() list(doc = tr_doc(), reg = test_registry())

add <- function(doc, reg, type, id = NULL, ...) {
  tr_doc_apply(doc, list(op = "add_node", type = type, id = id, ...), reg)
}

# Coleção de fluxo: os quatro papéis da região no menor formato possível —
# fonte, nó puro elevado, nó com memória e colapso. Mora aqui, e não no topo de
# um test-*.R, porque a detecção da região e o driver precisam dos MESMOS nós:
# duplicar a declaração deixaria os dois testarem grafos parecidos mas não
# iguais.
stream_collection <- function() {
  ponto <- function(...) tr_port("s/tab", stream = TRUE, ...)

  tr_collection(
    id = "s", version = "1.0.0", label = "Fluxo de teste",
    types = list(tr_type("s/tab", label = "Tabela")),
    nodes = list(
      tr_node("s/tabela", fn = function() NULL, outputs = list(out = "s/tab"),
              description = "Tabela comum, sem nada de fluxo."),
      # Espelha `data/to_stream`: recebe uma tabela por entrada COMUM (a aresta
      # que vira `external` da região) e emite pontos.
      tr_node("s/fonte", fn = function(dados) dados,
              inputs = list(dados = tr_port("s/tab", required = FALSE)),
              outputs = list(out = ponto()),
              description = "Parte a tabela em pontos."),
      tr_node("s/fonte_dupla", fn = function(dados) dados,
              inputs = list(dados = tr_port("s/tab", required = FALSE)),
              outputs = list(fluxo = ponto(), resumo = "s/tab"),
              description = "Emite pontos por uma saída e um resumo comum pela outra."),
      tr_node("s/puro", fn = function(x) x,
              inputs = list(x = "s/tab"), outputs = list(out = "s/tab"),
              description = "Nó comum: fora da região recebe tabela, dentro é elevado."),
      tr_node("s/acumula", fn = function(x, k) x,
              inputs = list(x = ponto(), k = tr_port("s/tab", required = FALSE)),
              outputs = list(out = ponto()),
              params = list(peso = tr_param_num(1)),
              init = function(peso) list(soma = 0, peso = peso),
              step = function(state, x) list(state = state, out = x),
              description = "Acumula ponto a ponto."),
      tr_node("s/junta", fn = function(a, b) a,
              inputs = list(a = "s/tab", b = "s/tab"), outputs = list(out = "s/tab"),
              description = "Combina duas tabelas — ou dois pontos, se elevado."),
      tr_node("s/colapsa", fn = function(x) x,
              inputs = list(x = ponto()), outputs = list(out = "s/tab"),
              description = "Junta os pontos num histórico."),
      tr_node("s/mostra", fn = function(x) invisible(x), inputs = list(x = "s/tab"),
              description = "Mostra a tabela recebida."),
      # Os três motivos de "não elevável", um por nó: a validação da região
      # recusa cada um separadamente, e a mensagem nomeia qual é.
      tr_node("s/impuro", fn = function(x) x,
              inputs = list(x = "s/tab"), outputs = list(out = "s/tab"),
              pure = FALSE, fingerprint = function(params) "ffff",
              description = "Lê o mundo fora do grafo."),
      tr_node("s/volatil", fn = function(x) x,
              inputs = list(x = "s/tab"), outputs = list(out = "s/tab"),
              volatile = TRUE,
              description = "Nunca cacheia entre execuções."),
      tr_node("s/contexto", fn = function(x, .ctx) x,
              inputs = list(x = "s/tab"), outputs = list(out = "s/tab"),
              description = "Precisa do contexto da unidade.")
    )
  )
}

stream_registry <- function() {
  reg <- tr_registry(); tr_use(stream_collection(), registry = reg); reg
}
