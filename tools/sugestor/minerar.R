# Minera as transições dos fluxos de exemplo para o sugestor.
#
# Uso: Rscript tools/sugestor/minerar.R (da raiz do repositório).
#
# Cada aresta `a -> b` de `exemplos/*/flows/*.json` vira um par
# (tipo de a, tipo de b). Os pares são separados pela coleção do `to` e
# gravados em `collections/<pacote>/inst/trama/transicoes.json`: quem é
# sugerido é o dono do dado, e é essa a regra que `tr_collection()` cobra.
# Laços do mesmo tipo são ignorados; sugerir o próprio bloco de novo não ajuda.

arquivos <- Sys.glob("exemplos/*/flows/*.json")
pares <- do.call(rbind, lapply(arquivos, function(f) {
  fl <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  tipo <- vapply(fl$nodes, function(n) n$type, "")
  do.call(rbind, lapply(fl$edges, function(e) {
    de <- unname(tipo[e$from$node]); para <- unname(tipo[e$to$node])
    if (is.na(de) || is.na(para) || de == para) return(NULL)
    data.frame(from = de, to = para)
  }))
}))

contagem <- aggregate(n ~ from + to, data = transform(pares, n = 1L), FUN = sum)
contagem$colecao <- sub("/.*$", "", contagem$to)

# O id da coleção nem sempre é o sufixo do pacote; o mapa sai do próprio
# `collection.R` de cada pacote, e não de uma lista escrita aqui.
pacotes <- list.dirs("collections", recursive = FALSE)
id_de <- vapply(pacotes, function(p) {
  src <- paste(readLines(file.path(p, "R", "collection.R")), collapse = "\n")
  m <- regmatches(src, regexpr("tr_collection\\(\\s*(id\\s*=\\s*)?\"[a-z0-9_]+\"", src))
  sub("^.*\"([a-z0-9_]+)\"$", "\\1", m)
}, "")

for (i in seq_along(pacotes)) {
  sel <- contagem[contagem$colecao == id_de[[i]], c("from", "to", "n")]
  sel <- sel[order(-sel$n, sel$from, sel$to), ]
  destino <- file.path(pacotes[[i]], "inst", "trama", "transicoes.json")
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(sel, destino, pretty = TRUE, auto_unbox = TRUE)
  cat(sprintf("%-26s %3d pares (%d transições)\n", basename(pacotes[[i]]), nrow(sel), sum(sel$n)))
}

orfas <- setdiff(unique(contagem$colecao), id_de)
if (length(orfas)) cat("Sem pacote para:", paste(orfas, collapse = ", "), "\n")
