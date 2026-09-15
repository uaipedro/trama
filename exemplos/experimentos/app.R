# Experimentos agronômicos — um frame por delineamento, cada um com o caminho
# completo de uma análise: ver os dados, ajustar, conferir os pressupostos,
# ler o quadro e comparar as médias. O fluxo é gerado por `construir.R`.
#
# Em desenvolvimento (fora de pacote instalado), núcleo e coleções entram por
# pkgload. Depois de instalados, isto vira duas linhas:
#   library(trama)
#   tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.models")))
# Rodável de qualquer lugar: `Rscript exemplos/experimentos/app.R` da raiz do
# repo, ou `Rscript app.R` daqui — os caminhos são relativos a ESTE arquivo.
setwd(local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[[1]]))) else normalizePath(getwd())
}))

pkgload::load_all("../..", quiet = TRUE, attach = TRUE)
pkgload::load_all("../../collections/trama.data", quiet = TRUE, attach = FALSE)
# A ordem importa: a `view` declara portas `data/table`, e a `models` declara
# portas `data/table` e `view/plot`. O registro recusa porta com tipo
# desconhecido, então carregar fora de ordem falha dizendo qual tipo faltou.
pkgload::load_all("../../collections/trama.view", quiet = TRUE, attach = FALSE)
pkgload::load_all("../../collections/trama.models", quiet = TRUE, attach = FALSE)

tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.models")))
