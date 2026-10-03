# Gera site/src/data/hero.json: a saída REAL dos blocos `data/example` e
# `data/summary` para os conjuntos que o fluxo vivo da home oferece. Os números
# que a home mostra saem do próprio trama, não de cópia à mão.
#
#   Rscript tools/site/export-hero.R
suppressMessages({
  pkgload::load_all(".", quiet = TRUE)
  pkgload::load_all("collections/trama.data", quiet = TRUE)
})

conjuntos <- list(
  mtcars = list(x = "wt", y = "mpg", cor = "cyl"),
  iris = list(x = "Petal.Length", y = "Petal.Width", cor = "Species"),
  airquality = list(x = "Temp", y = "Ozone", cor = "Month")
)

reg <- tr_registry()
tr_use(trama.data::trama_collection(), registry = reg)
store <- tr_store(tempfile("hero"))

saida <- lapply(names(conjuntos), function(nome) {
  fluxo <- tr_flow(reg) |>
    tr_add("carros", "data/example", dataset = nome) |>
    tr_add("resumo", "data/summary", from = "carros")
  doc <- tr_flow_doc(fluxo)
  tr_run(doc, registry = reg, store = store)
  dados <- as.data.frame(tr_value(doc, "carros", reg, store))
  resumo <- as.data.frame(tr_value(doc, "resumo", reg, store))
  eixo <- conjuntos[[nome]]
  pontos <- dados[stats::complete.cases(dados[c(eixo$x, eixo$y)]), ]
  list(
    nome = nome, eixos = eixo, linhas = nrow(dados), colunas = ncol(dados),
    cabeca = lapply(seq_len(min(6, nrow(dados))), function(i) lapply(dados[i, , drop = FALSE], function(v) if (is.factor(v)) as.character(v) else v)),
    nomes = names(dados),
    resumo = resumo,
    pontos = data.frame(x = pontos[[eixo$x]], y = pontos[[eixo$y]], g = as.character(pontos[[eixo$cor]]))
  )
})
names(saida) <- names(conjuntos)
jsonlite::write_json(saida, "site/src/data/hero.json", auto_unbox = TRUE, digits = 6, dataframe = "rows", pretty = FALSE)
cat("hero.json:", paste(names(saida), collapse = ", "), "\n")
