#!/usr/bin/env Rscript

# Exporta pressupostos e referências dos blocos para o site
# (site/src/data/node-docs.json). Diferente do catálogo do editor, os textos
# saem CRUS: string, ou objeto {pt, en} — o site escolhe o idioma da página.
# A referência de implementação ganha a versão do pacote instalado agora.
#
# Roda sozinho (Rscript tools/site/export-node-docs.R, da raiz) ou pelo
# export-node-visuals.R, que reaproveita o `registry` já montado.
stopifnot(file.exists("DESCRIPTION"), dir.exists("site/src/data"))
if (!exists("registry", inherits = FALSE)) {
  registry <- trama::tr_registry()
  for (package in c("trama.data", "trama.view", "trama.models", "trama.multi",
                    "trama.sampling", "trama.series", "trama.ml")) {
    trama::tr_use(package, registry = registry)
  }
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
})
