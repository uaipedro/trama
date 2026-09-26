#!/usr/bin/env Rscript

# Exporta pressupostos e referências dos blocos para o site
# (site/src/data/node-docs.json). Diferente do catálogo do editor, os textos
# saem CRUS: string, ou objeto {pt, en} — o site escolhe o idioma da página.
# A referência de implementação ganha a versão do pacote instalado agora.
#
# Roda sozinho (Rscript tools/site/export-node-docs.R, da raiz) ou pelo
# export-node-visuals.R, que reaproveita o `registry` já montado.
#
# Sozinho, carrega o trama e as coleções DA ÁRVORE (pkgload::load_all), não os
# instalados: um trama instalado antigo não tem pressupostos/referências e o
# site sairia vazio sem aviso. Ordem: data e view antes (as outras dependem).
stopifnot(file.exists("DESCRIPTION"), dir.exists("site/src/data"))
`%||%` <- function(a, b) if (is.null(a)) b else a
if (!exists("registry", inherits = FALSE)) {
  suppressMessages({
    pkgload::load_all(".", quiet = TRUE)
    colecoes <- c("trama.data", "trama.view", "trama.models", "trama.multi",
                  "trama.sampling", "trama.series", "trama.ml", "trama.experiments")
    for (package in colecoes) {
      pkgload::load_all(file.path("collections", package), quiet = TRUE,
                        export_all = FALSE, helpers = FALSE)
    }
    registry <- trama::tr_registry()
    for (package in colecoes) trama::tr_use(package, registry = registry)
  })
}
if (!exists("tr_pressuposto", envir = asNamespace("trama"))) {
  stop("O trama carregado (", getNamespaceInfo("trama", "path"), ") não tem ",
       "pressupostos/referências: rode da raiz com o trama da árvore.")
}

local({
  # Texto i18n cru: string fica string; lista nomeada vira objeto JSON.
  txt <- function(x) if (is.null(x)) NULL else if (is.list(x)) lapply(x, identity) else x
  sem_nulos <- function(x) x[!vapply(x, is.null, logical(1))]
  versao <- function(pacote) tryCatch(as.character(utils::packageVersion(pacote)),
                                      error = function(e) NULL)
  pressuposto <- function(p) sem_nulos(list(
    texto = txt(p$texto),
    verificar = if (length(p$verificar)) I(p$verificar),
    se_falhar = txt(p$se_falhar)
  ))
  referencia <- function(r) sem_nulos(list(
    papel = r$papel, autores = if (length(r$autores)) I(r$autores), ano = r$ano,
    titulo = txt(r$titulo), fonte = txt(r$fonte), doi = r$doi, url = r$url,
    pacote = r$pacote, funcao = r$funcao,
    versao = if (!is.null(r$pacote)) versao(r$pacote), nota = txt(r$nota)
  ))
  docs <- list()
  for (id in sort(names(registry$nodes))) {
    node <- registry$nodes[[id]]
    press <- node$pressupostos %||% list()
    refs <- node$referencias %||% list()
    if (!length(press) && !length(refs)) next
    docs[[id]] <- list(pressupostos = I(unname(lapply(press, pressuposto))),
                       referencias = I(unname(lapply(refs, referencia))))
  }
  # Sem nenhum bloco documentado, grava {} (e não []): o site lê um mapa.
  json <- if (length(docs)) jsonlite::toJSON(docs, auto_unbox = TRUE, pretty = TRUE) else "{}"
  writeLines(json, "site/src/data/node-docs.json", useBytes = TRUE)
  cat(length(docs), "blocos com pressupostos/referências exportados\n")
  if (!length(docs)) warning("Nenhum bloco exportado: o registry não tem pressupostos/referências.")
  # O Astro guarda o markdown renderizado em node_modules/.astro/data-store.json
  # e não sabe que as seções vêm deste JSON: sem apagar, dev/build servem as
  # páginas antigas.
  store <- "site/node_modules/.astro/data-store.json"
  if (file.exists(store)) unlink(store)
})
