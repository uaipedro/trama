#!/usr/bin/env Rscript
# Carrega as coleções da árvore, exporta o catálogo e delega a renderização ao
# mesmo módulo Markdown do editor (via Node, que é o ambiente de teste dele).
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE, export_all = TRUE))
colecoes <- c("trama.data", "trama.view", "trama.models", "trama.sampling", "trama.series",
              "trama.multi", "trama.ml", "trama.experiments", "trama.sql", "trama.python")
reg <- tr_registry()
for (p in colecoes) {
  caminho <- file.path("collections", p)
  if (!dir.exists(caminho)) next
  suppressPackageStartupMessages(pkgload::load_all(caminho, quiet = TRUE,
                                                   export_all = FALSE, attach = FALSE))
  tr_use(p, registry = reg)
}
arquivo <- tempfile(fileext = ".json")
writeLines(as.character(tr_catalog_json(reg)), arquivo, useBytes = TRUE)
status <- system2("node", c("tools/verificar-helps.mjs", shQuote(arquivo)))
unlink(arquivo)
if (!identical(status, 0L)) quit(status = if (is.null(status)) 1L else status)
