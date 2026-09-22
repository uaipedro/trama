#!/usr/bin/env Rscript

# Atualiza as miniaturas do site a partir do mesmo catálogo usado pelo editor.
packages <- c("trama.data", "trama.view", "trama.models", "trama.multi",
              "trama.sampling", "trama.series", "trama.ml")
# O script roda da raiz do repositório: Rscript tools/site/export-node-visuals.R.
stopifnot(file.exists("DESCRIPTION"), dir.exists("site/src/data"))
registry <- trama::tr_registry()
for (package in packages) trama::tr_use(package, registry = registry)

nodes <- lapply(registry$nodes[sort(names(registry$nodes))], function(node) {
  category <- registry$categories[[node$category]]
  if (is.null(category)) stop("Categoria ausente: ", node$category)
  visual <- list(
    accent = category$color,
    hasInput = length(node$inputs) > 0L,
    hasOutput = length(node$outputs) > 0L
  )
  if (!is.null(node$icon)) {
    if (node$icon$kind != "set") stop("Ícone SVG próprio requer suporte explícito: ", node$id)
    visual$icon <- node$icon$value
  }
  visual
})

json <- jsonlite::toJSON(nodes, auto_unbox = TRUE, pretty = TRUE)
writeLines(json, "site/src/data/node-visuals.json", useBytes = TRUE)
dir.create("site/public/icons", recursive = TRUE, showWarnings = FALSE)
file.copy("inst/www/vendor/lucide.svg", "site/public/icons/lucide.svg", overwrite = TRUE)
cat(length(nodes), "blocos exportados\n")
