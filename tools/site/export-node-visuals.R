#!/usr/bin/env Rscript

# Atualiza as miniaturas do site a partir do mesmo catálogo usado pelo editor.
packages <- c("trama.data", "trama.view", "trama.models", "trama.multi",
              "trama.sampling", "trama.series", "trama.ml")
# O script roda da raiz do repositório: Rscript tools/site/export-node-visuals.R.
stopifnot(file.exists("DESCRIPTION"), dir.exists("site/src/data"))
`%||%` <- function(a, b) if (is.null(a)) b else a
registry <- trama::tr_registry()
for (package in packages) trama::tr_use(package, registry = registry)

# Cor de papel do tema CLARO do editor (o site é claro), lida do próprio
# trama.css: as coleções não mandam mais cor de categoria, só o papel.
css <- paste(readLines("inst/www/trama.css", warn = FALSE), collapse = "\n")
claro <- regmatches(css, regexpr(':root\\[data-tema="claro"\\]\\{[^}]*\\}', css))
achados <- regmatches(claro, gregexpr("--tr-papel-([a-z]+):(#[0-9a-fA-F]+)", claro))[[1]]
cor_papel <- setNames(sub(".*:", "", achados), sub("--tr-papel-([a-z]+):.*", "\\1", achados))

nodes <- lapply(registry$nodes[sort(names(registry$nodes))], function(node) {
  category <- registry$categories[[node$category]]
  if (is.null(category)) stop("Categoria ausente: ", node$category)
  visual <- list(
    accent = category$color %||% unname(cor_papel[node$role %||% category$role %||% ""]) %||% "#64748b",
    hasInput = length(node$inputs) > 0L,
    hasOutput = length(node$outputs) > 0L,
    label = node$label,
    # Etapa do bloco na coleção: o catálogo do site agrupa por ela, na ordem
    # em que a coleção registra as categorias.
    category = node$category,
    categoryLabel = category$label,
    categoryOrder = match(node$category, names(registry$categories))
  )
  # Cor da primeira porta de cada lado, pelo tipo: é a cor da seta no editor.
  port_color <- function(ports) {
    if (length(ports) == 0L) return(NULL)
    registry$types[[ports[[1]]$type]]$color
  }
  visual$inputColor <- port_color(node$inputs)
  visual$outputColor <- port_color(node$outputs)
  # O card estático do site (flow-canvas-html.ts) desenha o bloco como o
  # editor: papel (cor do cabeçalho pelo token do tema), portas pelo nome e a
  # lista de params com o widget de cada tipo.
  papel <- node$role %||% category$role
  if (!is.null(papel)) visual$role <- papel
  porta <- function(nome, p) list(name = nome, color = registry$types[[p$type]]$color %||% "#64748b",
                                  required = isTRUE(p$required), multiple = isTRUE(p$multiple))
  visual$inputs <- unname(Map(porta, names(node$inputs), node$inputs))
  visual$outputs <- unname(Map(porta, names(node$outputs), node$outputs))
  visual$params <- unname(Map(function(nome, p) {
    x <- list(name = nome, kind = p$kind, label = p$label %||% nome)
    if (!is.null(p$default) && length(p$default) == 1L) x$default <- p$default
    if (!is.null(p$example)) x$example <- p$example
    if (!is.null(p$choices)) x$choices <- I(as.character(p$choices))
    x
  }, names(node$params), node$params))
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
