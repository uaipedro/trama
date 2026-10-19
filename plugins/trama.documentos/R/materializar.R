# A materialização: `_trama/<chave>/` ao lado do documento.
#
# A pasta é do trama (a pessoa não edita); a região só a referencia. Cada
# pasta leva um manifesto `trama.json` com o hash do que foi materializado e
# a chave do store de onde veio. O render lê só a pasta: não recalcula, não
# precisa do store (o GC dele não apaga nada daqui) e roda no GitHub Actions.

.TD_PASTA <- "_trama"

#' Hash do resultado materializado: corpo da região e arquivos, em ordem.
#' @noRd
.td_hash_parte <- function(parte) {
  arqs <- parte$arquivos[order(names(parte$arquivos))]
  partes <- c(paste(parte$corpo, collapse = "\n"),
              vapply(names(arqs), function(n) {
                paste0(n, ":", digest::digest(arqs[[n]], algo = "sha256", serialize = FALSE))
              }, ""))
  digest::digest(paste(partes, collapse = "\n"), algo = "sha256", serialize = FALSE)
}

.td_pasta_regiao <- function(pasta_doc, chave) file.path(pasta_doc, .TD_PASTA, chave)

.td_manifesto <- function(pasta_doc, chave) {
  f <- file.path(.td_pasta_regiao(pasta_doc, chave), "trama.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
}

#' Grava a pasta inteira de uma vez: monta ao lado e troca, para o render
#' nunca ver metade de uma atualização.
#' @noRd
.td_materializar <- function(pasta_doc, chave, parte, manifesto) {
  base <- file.path(pasta_doc, .TD_PASTA)
  dir.create(base, showWarnings = FALSE, recursive = TRUE)
  nova <- tempfile(paste0(".", chave, "-"), tmpdir = base)
  dir.create(nova)
  for (n in names(parte$arquivos)) writeBin(parte$arquivos[[n]], file.path(nova, n))
  manifesto$arquivos <- as.list(names(parte$arquivos))
  writeLines(jsonlite::toJSON(manifesto, auto_unbox = TRUE, pretty = TRUE, null = "null"),
             file.path(nova, "trama.json"), useBytes = TRUE)
  destino <- .td_pasta_regiao(pasta_doc, chave)
  velha <- NULL
  if (dir.exists(destino)) {
    velha <- tempfile(paste0(".", chave, "-velha-"), tmpdir = base)
    file.rename(destino, velha)
  }
  if (!file.rename(nova, destino)) {
    if (!is.null(velha)) file.rename(velha, destino)
    unlink(nova, recursive = TRUE)
    .td_erro(sprintf("N\u{E3}o foi poss\u{ED}vel gravar '%s'.", destino), "tr_documento_erro_caminho")
  }
  if (!is.null(velha)) unlink(velha, recursive = TRUE)
  invisible(destino)
}

#' Só a chave do store no manifesto (o resultado não mudou).
#' @noRd
.td_manifesto_chave <- function(pasta_doc, chave, chave_store) {
  f <- file.path(.td_pasta_regiao(pasta_doc, chave), "trama.json")
  m <- .td_manifesto(pasta_doc, chave)
  m$chave_store <- chave_store
  writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE, null = "null"), f, useBytes = TRUE)
}
