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
