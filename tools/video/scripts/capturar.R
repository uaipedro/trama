# Captura os blocos de um roteiro no trama DE VERDADE, pelo canal de controle.
#
#   TRAMA_DEV=<repo> Rscript scripts/capturar.R capturas/<id>.fluxo.json
#
# Precisa de um editor aberto (tr_app() + aba no navegador) num projeto
# descartável: o fluxo é montado nele. Grava capturas/<id>.json com, por nó,
# a entrada do catálogo (spec real: rótulo, params, escolhas, portas) e o
# `result` (preview que o card desenha; PNG copiado para public/capturas/).
# O motor lê isso em vez de números transcritos à mão.
args <- commandArgs(TRUE)
if (length(args) != 1L) stop("Uso: Rscript scripts/capturar.R capturas/<id>.fluxo.json")
dev <- Sys.getenv("TRAMA_DEV")
if (nzchar(dev)) suppressMessages(pkgload::load_all(dev, quiet = TRUE)) else library(trama)
ns <- asNamespace("trama")
`%||%` <- function(a, b) if (is.null(a)) b else a
projeto <- Sys.getenv("TRAMA_PROJETO")
cx <- ns$.tr_cli_conexao(if (nzchar(projeto)) projeto)
http <- function(...) {
  r <- ns$.tr_cli_http(cx, ...)
  if (!isTRUE(r$ok)) stop(r$message %||% "recusado", call. = FALSE)
  r
}

fluxo <- jsonlite::read_json(args[[1]], simplifyVector = FALSE)
id <- fluxo$id
# Recapturar é rodar de novo: nó que já existe com o mesmo id sai antes.
existentes <- vapply(http("GET", "/state")$nodes, function(n) n$id, "")
for (passo in fluxo$passos) {
  if (identical(passo$cmd, "add") && passo$id %in% existentes) {
    http("POST", "/cmd", list(cmd = "rm", node = passo$id))
  }
}
# Cada passo é o corpo de um POST /cmd (`add`, `set`, `link`), igual ao CLI.
for (passo in fluxo$passos) http("POST", "/cmd", passo)

png_dir <- file.path("public", "capturas", id)
dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)
nos <- list()
for (no in fluxo$capturar) {
  r <- ns$.tr_cli_result(cx, no, wait = 120)
  if (!r$status %in% c("done", "cached")) stop(sprintf("'%s' terminou em %s: %s", no, r$status, r$message %||% ""))
  for (porta in names(r$outputs)) {
    f <- r$outputs[[porta]]$preview$files
    for (ext in names(f)) {
      destino <- file.path(png_dir, sprintf("%s-%s.%s", no, porta, ext))
      file.copy(f[[ext]], destino, overwrite = TRUE)
      # Caminho relativo a public/, como o Remotion serve (staticFile()).
      r$outputs[[porta]]$preview$files[[ext]] <- sub("^public/", "", destino)
    }
  }
  estado <- Filter(function(n) n$id == no, http("GET", "/state")$nodes)[[1]]
  nos[[no]] <- list(
    catalogo = http("GET", "/catalog", query = list(tipo = estado$type))$node,
    params = estado$params,
    resultado = r[c("status", "outputs")]
  )
}
saida <- file.path(dirname(args[[1]]), paste0(id, ".json"))
jsonlite::write_json(list(id = id, trama = as.character(utils::packageVersion("trama")),
                          capturado_em = format(Sys.time(), "%Y-%m-%d"), nos = nos),
                     saida, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
cat("gravado:", saida, "\n")
