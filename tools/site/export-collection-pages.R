#!/usr/bin/env Rscript

# Gera as páginas de bloco de uma coleção no site a partir da ajuda dos nós
# (o mesmo texto que o editor mostra). As miniaturas do site
# (node-visuals.json) são do tools/site/export-node-visuals.R.
#
#   Rscript tools/site/export-collection-pages.R trama.experiments experimentos
#
# Roda da raiz do repositório com a coleção instalada (R_LIBS). Reescreve só
# as páginas de bloco (<pasta>/<bloco>.md); guias escritos à mão, como
# visao-geral.md, ficam intactos.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("uso: export-collection-pages.R <pacote> <pasta-do-site>")
pacote <- args[[1]]; pasta <- args[[2]]
stopifnot(file.exists("DESCRIPTION"), dir.exists("site/src/data"))

registry <- trama::tr_registry()
for (p in unique(c("trama.data", "trama.view", "trama.models", pacote))) {
  if (p %in% rownames(utils::installed.packages())) trama::tr_use(p, registry = registry)
}
prefixo <- sub("^trama\\.", "", pacote)
ids <- sort(grep(paste0("^", prefixo, "/"), names(registry$nodes), value = TRUE))
if (!length(ids)) stop("Nenhum bloco ", prefixo, "/ no registro.")

pt <- function(x) if (is.list(x)) x$pt %||% x[[1]] else x
`%||%` <- function(a, b) if (is.null(a)) b else a
aspas <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')

saida <- file.path("site/src/content/docs/colecoes", pasta)
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
ordem_cat <- c()
for (id in ids) {
  node <- registry$nodes[[id]]
  ajuda <- pt(node$help)
  categoria <- pt(registry$categories[[node$category]]$label %||% node$category)
  # "Veja também" dá os relacionados: todo id de bloco citado ali.
  veja <- regmatches(ajuda, regexpr("## Veja também[\\s\\S]*?(\n## |$)", ajuda, perl = TRUE))
  relacionados <- unique(unlist(regmatches(veja, gregexpr("`[a-z_]+/[a-z_0-9]+`", veja))))
  relacionados <- setdiff(gsub("`", "", relacionados), id)
  corpo <- sub("^## Descrição", "## O que o bloco faz", trimws(ajuda))
  md <- c(
    "---",
    paste0("title: ", aspas(pt(node$label))),
    paste0("description: ", aspas(pt(node$description))),
    "section: colecoes",
    paste0("collection: ", pasta),
    paste0("node: ", id),
    paste0("category: ", aspas(categoria)),
    paste0("related: [", paste(relacionados, collapse = ", "), "]"),
    "---",
    "",
    "<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->",
    "",
    corpo,
    ""
  )
  writeLines(md, file.path(saida, paste0(sub(".*/", "", id), ".md")), useBytes = TRUE)
}

# Miniaturas (site/src/data/node-visuals.json) têm dono próprio:
# tools/site/export-node-visuals.R, que grava o formato completo (papel,
# portas, params). Este script não mexe nelas.
cat(length(ids), "páginas de", pacote, "em", saida, "\n")
cat("Miniaturas: rode tools/site/export-node-visuals.R se params/portas mudaram.\n")
