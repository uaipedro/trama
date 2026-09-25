# Glossário de parâmetros das coleções: trava de nomes.
#
# Por quê: o catálogo acumulou o mesmo conceito com nomes diferentes (alfa vs
# nivel vs confianca, alvo vs resposta...). Quem aprende um nó deve reconhecer o
# parâmetro no próximo. Este teste lê o catálogo real das 7 coleções e falha
# listando `id: param` de cada nome proibido. A tabela e a justificativa vivem em
# docs/glossario-parametros.md.

colecoes_glossario <- c(
  "trama.data", "trama.view", "trama.models", "trama.series",
  "trama.multi", "trama.sampling", "trama.ml"
)

# Proibidos em qualquer nó, pelo nome (proibido -> canônico).
params_proibidos <- c(
  alfa = "confianca",
  nivel = "confianca",
  alvo = "resposta",
  coluna = "variavel"
)

# Porta de entrada proibida em qualquer nó (proibida -> canônica).
entradas_proibidas <- c(data = "dados")

# `cols` e `grupo` são válidos quando significam "colunas quaisquer" e
# "agrupamento"; só são proibidos quando fazem papel de preditoras/resposta.
# Isso não se deduz do nome, então a lista é explícita e derivada do catálogo
# (ajuda e código de cada nó).
usos_proibidos <- list(
  list(id = "multi/discriminant", param = "grupo", canonico = "resposta"),
  list(id = "multi/discriminant", param = "cols", canonico = "preditores"),
  list(id = "multi/logistic", param = "grupo", canonico = "resposta"),
  list(id = "multi/logistic", param = "cols", canonico = "preditores"),
  list(id = "ml/linear", param = "cols", canonico = "preditores"),
  list(id = "ml/cart", param = "cols", canonico = "preditores"),
  list(id = "ml/figs", param = "cols", canonico = "preditores"),
  list(id = "ml/forest", param = "cols", canonico = "preditores"),
  list(id = "ml/svm", param = "cols", canonico = "preditores"),
  list(id = "ml/xgboost", param = "cols", canonico = "preditores"),
  list(id = "ml/tune", param = "cols", canonico = "preditores")
)

# Homônimos com outro sentido: o nome proibido aqui não é o conceito do
# glossário, e trocá-lo pelo canônico estaria errado.
excecoes <- c(
  "models/anova_dql: coluna",     # fator coluna do quadrado latino
  "models/chisq: coluna",         # variável nas colunas da tabela de contingência
  "models/fisher_exact: coluna",  # idem
  "sampling/proportion: nivel"    # categoria da variável, não nível de confiança
)

violacoes_glossario <- function(nos) {
  nomes <- function(l) vapply(l, function(p) p$name, "")
  out <- character()
  for (n in nos) {
    ps <- nomes(n$params)
    for (p in intersect(ps, names(params_proibidos))) {
      if (paste0(n$id, ": ", p) %in% excecoes) next
      out <- c(out, sprintf("%s: param %s -> %s", n$id, p, params_proibidos[[p]]))
    }
    for (e in intersect(nomes(n$inputs), names(entradas_proibidas))) {
      out <- c(out, sprintf("%s: entrada %s -> %s", n$id, e, entradas_proibidas[[e]]))
    }
    for (u in usos_proibidos) {
      if (identical(u$id, n$id) && u$param %in% ps) {
        out <- c(out, sprintf("%s: param %s -> %s", n$id, u$param, u$canonico))
      }
    }
  }
  sort(out)
}

test_that("parâmetros e portas seguem o glossário", {
  skip("glossário: renomes nas tasks 1.2–1.4")
  for (p in colecoes_glossario) skip_if_not_installed(p)
  for (p in colecoes_glossario) tr_use(p)

  v <- violacoes_glossario(tr_catalog()$nodes)
  expect(
    length(v) == 0,
    paste0("Nomes fora do glossário (docs/glossario-parametros.md):\n",
           paste0("  ", v, collapse = "\n"))
  )
})
