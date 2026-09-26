# Delineamentos — o caso de validação da trama.experiments: parcela
# subdividida com a resposta composta termo a termo e analisada de dois jeitos,
# com e sem erro de parcela. O fluxo é gerado por `construir.R`.
#
# Depois de instalados, isto vira duas linhas:
#   library(trama)
#   tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.models", "trama.experiments")))
setwd(local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[[1]]))) else normalizePath(getwd())
}))

# Ordem das dependências: data, view, models, experiments.
colecoes <- c("trama.data", "trama.view", "trama.models", "trama.experiments")
pkgload::load_all("../..", quiet = TRUE, attach = TRUE)
for (p in colecoes) pkgload::load_all(file.path("../../collections", p), quiet = TRUE, attach = FALSE)

tr_app(tr_project(".", collections = colecoes))
