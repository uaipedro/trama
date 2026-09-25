# Régua de cobertura: todo bloco inferencial tem que dizer o que supõe e de
# onde vem. Um bloco é "exigido" quando o papel efetivo (o do nó, senão o da
# categoria) é `ajuste` ou `avaliacao` — é onde moram modelos, testes,
# estimadores e previsões — tirando os gráficos puros (id com `plot`). Leitura,
# preparação, origem, inspeção e saída ficam fora.
#
# O papel sozinho erra em poucos casos, listados abaixo à mão:
# - comparações múltiplas e médias ajustadas são `leitura` de um ajuste, mas
#   cada uma é um procedimento inferencial com pressupostos próprios;
# - `series/forecast` é `leitura` do modelo, mas é a previsão (com intervalo);
# - `multi/kmo_bartlett` está em `inspecao`, mas inclui o teste de Bartlett;
# - `models/anova_split_plot` tem `plot` no id (é "split plot", não gráfico);
# - `models/anova_table` é `leitura`, mas faz os testes F de cada termo;
# - em `sampling`, os tamanhos de amostra e a calibração são `preparacao`, mas
#   dependem de fórmulas de variância com pressupostos próprios.
#
# Documentado = ≥1 pressuposto, ≥1 referência de papel `teoria` ou
# `livro-texto` e ≥1 de papel `implementacao`.
#
# Os ainda não documentados ficam em fixtures/docs-pendentes.txt, que só pode
# encolher. Para regenerar (só ao documentar blocos, nunca para acomodar um
# bloco novo): TRAMA_REGERAR_PENDENTES=1 e rodar este arquivo.

.cob_incluir <- c("models/duncan", "models/emmeans", "models/linear_hypothesis",
                  "models/pairwise", "models/waller_duncan", "series/forecast",
                  "multi/kmo_bartlett", "models/anova_split_plot",
                  "models/anova_table",
                  "sampling/size_mean", "sampling/size_proportion", "sampling/size_stratified",
                  "sampling/size_cluster", "sampling/size_domains", "sampling/poststratify",
                  "sampling/rake")

.cob_colecoes <- c("trama.data", "trama.view", "trama.models", "trama.sampling",
                   "trama.series", "trama.multi", "trama.ml")

cob_registry <- function() {
  raiz <- "../../collections"
  for (p in .cob_colecoes) skip_if_not(dir.exists(file.path(raiz, p)), paste("coleção", p, "ausente"))
  reg <- tr_registry()
  # A ordem importa: data e view primeiro, as outras dependem dos tipos delas.
  for (p in .cob_colecoes) {
    suppressMessages(pkgload::load_all(file.path(raiz, p), quiet = TRUE,
                                       export_all = FALSE, attach = FALSE))
    tr_use(p, registry = reg)
  }
  reg
}

cob_exigidos <- function(reg) {
  ids <- sort(ls(reg$nodes))
  papel <- vapply(ids, function(id) {
    n <- reg$nodes[[id]]
    n$role %||% reg$categories[[n$category]]$role %||% ""
  }, character(1))
  regra <- papel %in% c("ajuste", "avaliacao") & !grepl("plot", ids)
  sort(unique(c(ids[regra], intersect(.cob_incluir, ids))))
}

cob_documentado <- function(n) {
  papeis <- vapply(n$referencias, function(r) r$papel, character(1))
  length(n$pressupostos) >= 1L &&
    any(papeis %in% c("teoria", "livro-texto")) &&
    any(papeis == "implementacao")
}

cob_ler_pendentes <- function(p) {
  x <- trimws(readLines(p, warn = FALSE))
  x[nzchar(x) & !startsWith(x, "#")]
}

test_that("blocos inferenciais têm pressupostos e referências (ou estão pendentes)", {
  for (p in .cob_colecoes) skip_if_not_installed(p)
  reg <- cob_registry()
  exigidos <- cob_exigidos(reg)
  expect_true(all(c("models/t_test", "models/lm", "models/anova_dbc", "sampling/mean",
                    "series/arima", "multi/pca", "ml/forest") %in% exigidos))
  expect_false(any(c("data/read_csv", "data/filter", "view/histogram",
                     "models/example", "series/plot") %in% exigidos))
  expect_true(all(.cob_incluir %in% ls(reg$nodes)))

  falta <- exigidos[!vapply(exigidos, function(id) cob_documentado(reg$nodes[[id]]), logical(1))]
  arq <- test_path("fixtures", "docs-pendentes.txt")

  if (nzchar(Sys.getenv("TRAMA_REGERAR_PENDENTES"))) {
    writeLines(c(
      "# Blocos inferenciais ainda sem pressupostos e referências (teoria ou",
      "# livro-texto + implementacao). Gerado por test-cobertura-docs.R.",
      "# Esta lista só pode ENCOLHER: documente o bloco e tire a linha dele.",
      "# Bloco novo exigido nasce documentado; não entra aqui.",
      sort(falta)), arq)
  }

  pendentes <- cob_ler_pendentes(arq)
  sumiram <- setdiff(pendentes, ls(reg$nodes))
  expect(length(sumiram) == 0L, paste0(
    "Ids em docs-pendentes.txt que não existem mais (tire-os): ",
    paste(sumiram, collapse = ", ")))
  novos <- setdiff(falta, pendentes)
  expect(length(novos) == 0L, paste0(
    "Blocos inferenciais sem pressupostos/referências (documente-os): ",
    paste(novos, collapse = ", ")))
  feitos <- intersect(setdiff(pendentes, falta), exigidos)
  expect(length(feitos) == 0L, paste0(
    "Blocos já documentados ainda em docs-pendentes.txt (tire-os da lista): ",
    paste(feitos, collapse = ", ")))
  fora <- setdiff(intersect(pendentes, ls(reg$nodes)), exigidos)
  expect(length(fora) == 0L, paste0(
    "Ids em docs-pendentes.txt que não são exigidos (tire-os): ",
    paste(fora, collapse = ", ")))
})
