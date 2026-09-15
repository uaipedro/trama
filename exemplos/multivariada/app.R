# Análise multivariada — as três técnicas da coleção, cada uma no conjunto
# que a ensina: componentes principais nos estados dos EUA, análise fatorial no
# questionário com as cargas plantadas (varimax ao lado de oblimin), e
# discriminante nos vinhos, com M de Box, funções e validação cruzada; logística
# contra LDA no pima, com ROC; e o jackknife das cargas da PCA.
#
# Em desenvolvimento (fora de pacote instalado), núcleo e coleções entram por
# pkgload. Depois de instalados, isto vira duas linhas:
#   library(trama)
#   tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.multi")))
# Rodável de qualquer lugar: `Rscript exemplos/multivariada/app.R` da raiz do
# repo, ou `Rscript app.R` daqui — os caminhos são relativos a ESTE arquivo.
setwd(local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[[1]]))) else normalizePath(getwd())
}))

pkgload::load_all("../..", quiet = TRUE, attach = TRUE)
pkgload::load_all("../../collections/trama.data", quiet = TRUE, attach = FALSE)
# A ordem importa e não é estilo: a `view` declara portas `data/table`, e a
# `multi` declara portas `data/table` e `view/plot`. O registro recusa porta
# com tipo desconhecido, então carregar fora de ordem falha dizendo qual tipo
# faltou.
pkgload::load_all("../../collections/trama.view", quiet = TRUE, attach = FALSE)
pkgload::load_all("../../collections/trama.multi", quiet = TRUE, attach = FALSE)

tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.multi")))
