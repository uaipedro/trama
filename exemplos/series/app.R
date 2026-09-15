# Séries temporais — AirPassengers do começo ao fim: a série, a sazonalidade,
# a decomposição, o correlograma da série estacionarizada, um ETS contra o
# ingênuo sazonal com treino e teste, e os resíduos testados.
#
# Em desenvolvimento (fora de pacote instalado), núcleo e coleções entram por
# pkgload. Depois de instalados, isto vira duas linhas:
#   library(trama)
#   tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.series")))
# Rodável de qualquer lugar: `Rscript exemplos/series/app.R` da raiz do repo,
# ou `Rscript app.R` daqui — os caminhos são relativos a ESTE arquivo.
setwd(local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[[1]]))) else normalizePath(getwd())
}))

pkgload::load_all("../..", quiet = TRUE, attach = TRUE)
pkgload::load_all("../../collections/trama.data", quiet = TRUE, attach = FALSE)
# A ordem importa e não é estilo: a `view` declara portas `data/table`, e a
# `series` declara portas `data/table` e `view/plot`. O registro recusa porta
# com tipo desconhecido, então carregar fora de ordem falha dizendo qual tipo
# faltou.
pkgload::load_all("../../collections/trama.view", quiet = TRUE, attach = FALSE)
pkgload::load_all("../../collections/trama.series", quiet = TRUE, attach = FALSE)

tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.series")))
