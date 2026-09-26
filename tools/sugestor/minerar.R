# Minera as transições dos fluxos de exemplo para o sugestor.
#
# Uso: Rscript tools/sugestor/minerar.R [fontes] (da raiz do repositório).
#
# Os fluxos vêm de `tools/sugestor/corpus.mjs`, que junta e deduplica as
# fontes reais do repositório: exemplos, templates das coleções e os fluxos
# em blocos ```r da doc do site. `fontes` (separadas por vírgula) restringe
# o corpus; sem argumento, usa todas.
#
# Cada aresta `a -> b` de cada fluxo vira um par
# (tipo de a, tipo de b). Os pares são separados pela coleção do `to` e
# gravados em `collections/<pacote>/inst/trama/transicoes.json`: quem é
# sugerido é o dono do dado, e é essa a regra que `tr_collection()` cobra.
# Laços do mesmo tipo são ignorados; sugerir o próprio bloco de novo não ajuda.

fontes <- commandArgs(trailingOnly = TRUE)
corpus <- jsonlite::fromJSON(
  paste(system2("node", c("--experimental-strip-types", "--no-warnings",
                          "tools/sugestor/corpus.mjs", fontes), stdout = TRUE),
        collapse = ""),
  simplifyVector = FALSE
)
cat(sprintf("%d fluxos no corpus\n", length(corpus)))

# Os fluxos do corpus podem estar gravados com ids antigos (`ml/evaluate`,
# `multi/roc`...): a mineração lê o fluxo JÁ MIGRADO pelas coleções, senão o
# corpus sugeriria blocos que não existem mais — e a `to` de outra coleção
# (o bloco mudou de casa) cairia no arquivo errado.
reg <- trama::tr_registry()
suppressMessages(for (p in c("trama.data", "trama.view", "trama.models", "trama.ml",
                             "trama.multi", "trama.series", "trama.sampling")) {
  trama::tr_use(p, registry = reg)
})
pares <- do.call(rbind, lapply(corpus, function(fl) {
  fl <- tryCatch(trama::tr_doc_migrate(fl, reg), error = function(e) fl)
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
