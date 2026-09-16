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
      #
      # `dados = NULL` no formal, e não `function(dados)`: a porta é OPCIONAL, e
      # porta opcional solta não é passada — o `fn` cai no próprio default
      # (`.tr_run_unit()`). Sem o default, o nó falhava com "argumento ausente,
      # sem padrão" assim que alguém o executasse com a porta solta, que é
      # exatamente o grafo dos testes de região. Até a Fase 3 nada executava
      # esses nós e o defeito ficou latente; o driver o encontrou.
      tr_node("s/fonte", fn = function(dados = NULL) dados,
              inputs = list(dados = tr_port("s/tab", required = FALSE)),
              outputs = list(out = ponto()),
              description = "Parte a tabela em pontos."),
      tr_node("s/fonte_dupla", fn = function(dados = NULL) dados,
              inputs = list(dados = tr_port("s/tab", required = FALSE)),
              outputs = list(fluxo = ponto(), resumo = "s/tab"),
              description = "Emite pontos por uma saída e um resumo comum pela outra."),
      # Fonte que gera os PRÓPRIOS pontos. `s/fonte` com a entrada solta devolve
      # NULL, que é zero pontos — e num fluxo de zero pontos `step` nunca roda.
      # Testar "membro que explode num passo" exige fluxo não-vazio sem depender
      # de um nó de fora da região.
      tr_node("s/pontos", fn = function(n) as.list(seq_len(n)),
              outputs = list(out = ponto()),
              params = list(n = tr_param_int(3L)),
              description = "Emite n pontos."),
      # Falha DENTRO do laço, e não na montagem: é a falha da unidade-região que
      # o scheduler tem que propagar por TODOS os colapsos dela.
      tr_node("s/acumula_explode", fn = function(x) x,
              inputs = list(x = ponto()), outputs = list(out = ponto()),
              init = function() list(i = 0L),
              step = function(state, x) rlang::abort("explodiu no passo",
                                                     class = "tr_error_teste"),
              description = "Acumula até explodir."),
      # Nó COMUM que falha, a montante de uma região: é o que faz a região cair
      # como vítima da poda, em vez de ser ela a falhar.
      tr_node("s/explode", fn = function() rlang::abort("explodiu", class = "tr_error_teste"),
              outputs = list(out = "s/tab"),
              description = "Tabela que não sai."),
      tr_node("s/puro", fn = function(x) x,
              inputs = list(x = "s/tab"), outputs = list(out = "s/tab"),
              description = "Nó comum: fora da região recebe tabela, dentro é elevado."),
      # `k = NULL` no formal pela mesma razão de `s/fonte` acima: `k` é porta
      # OPCIONAL, e porta opcional solta não é passada. Sem o default, o nó
      # aborta "argumento ausente, sem padrão" no instante em que o corpo do
      # `fn` tocar `k` — hoje não toca (o corpo é `x`), e é só isso que separa
      # este nó dos dois que já quebraram. `portas_opcionais_sem_default()`
      # guarda a invariante pra que o próximo nó da coleção não nasça assim.
      tr_node("s/acumula", fn = function(x, k = NULL) x,
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
              description = "Precisa do contexto da unidade."),
      # Nó com memória que ACUMULA os três motivos de "não elevável". Existe pra
      # provar que a isenção do `online` na validação é o que decide: sem ela,
      # um nó que roda por `init`/`step` — contrato próprio, não elevação —
      # seria recusado por ser impuro, e nenhum outro nó da coleção exercita
      # esse caminho.
      tr_node("s/acumula_impuro", fn = function(x, .ctx) x,
              inputs = list(x = ponto()), outputs = list(out = ponto()),
              pure = FALSE, volatile = TRUE, fingerprint = function(params) "eeee",
              init = function() list(n = 0),
              step = function(state, x) list(state = state, out = x),
              description = "Acumula lendo o mundo fora do grafo.")
    )
  )
}

stream_registry <- function() {
  reg <- tr_registry(); tr_use(stream_collection(), registry = reg); reg
}

# Os formais de `fn`/`init`/`step` que correspondem a porta `required = FALSE` e
# NÃO têm default — devolvidos como "<nó> <função> <formal>".
#
# É uma classe de defeito, e não um caso isolado: porta opcional solta não é
# passada (`.tr_run_unit()` e `.tr_region_args()` param de propósito, pra que o
# `fn` caia no próprio default), então o formal sem default aborta "argumento
# ausente, sem padrão" no instante em que o corpo tocar nele — longe da causa, e
# só num grafo que a validação ACEITA. Nasce calado porque um corpo que ignora o
# argumento nunca o força: a coleção parece sadia até alguém editar o corpo.
# Guardado nas coleções de TESTE porque foi lá que os três instances moraram.
portas_opcionais_sem_default <- function(col) {
  out <- character()
  for (nd in col$nodes) {
    opt <- names(Filter(function(p) !isTRUE(p$required), nd$inputs))
    # `init` está aqui por cinto-e-suspensório, não por necessidade: hoje
    # `tr_node()` já recusa formal de `init` que não seja param
    # (`tr_error_bad_init`), e input e param não podem dividir nome
    # (`tr_error_name_collision`), então este braço não dispara. Fica para o dia
    # em que alguém afrouxar aquela validação.
    for (fname in c("fn", "init", "step")) {
      f <- nd[[fname]]
      if (!is.function(f)) next
      fo <- formals(f)
      for (a in intersect(names(fo), opt)) {
        if (identical(fo[[a]], quote(expr = ))) out <- c(out, paste(nd$id, fname, a))
      }
    }
  }
  out
}
