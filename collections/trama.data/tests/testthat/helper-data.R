# A maior parte dos nós se testa pelo NÍVEL 1: `tr_filter(df, "x > 1")` é
# chamada de função R comum, sem registro e sem motor. É de propósito — se o
# nó só se testa através do grafo, ele deixou de ser função R comum, que é a
# promessa que o pacote faz.

df_exemplo <- function() {
  tibble::tibble(
    regiao  = c("sul", "sul", "norte", "norte", "sul", "norte"),
    produto = c("a", "b", "a", "b", "a", "a"),
    valor   = c(10, 20, 30, 40, 5, 15),
    qtd     = c(1L, 2L, 3L, 4L, 1L, 2L)
  )
}

# Só para os testes que precisam do registro de verdade (catálogo, ajuda).
data_registry <- function() {
  reg <- trama::tr_registry()
  trama::tr_use(trama_collection(), registry = reg)
  reg
}

# Avalia o código gerado por `tr_flow_code()`. O código é a DSL como o usuário
# a escreveria — `tr_add(...)` sem `trama::` —, então qualificar as chamadas não
# é opção: quem as escreve é o gerador. O que decide se elas resolvem é o PAI do
# ambiente de avaliação. Sob `pkgload::load_all()` o namespace do trama fica
# anexado e `parent = environment()` bastava; sob `R CMD check`, que é o
# ambiente real, só `trama.data` está anexado e o mesmo teste morria com
# `could not find function "tr_add"`. Amarrar o pai ao namespace do trama torna
# o teste independente de quem está anexado — e é o único caminho para avaliar
# código gerado, então todo round-trip novo passa por aqui.
avaliar_codigo <- function(code, reg) {
  eval(parse(text = code), envir = list2env(list(reg = reg), parent = asNamespace("trama")))
}
