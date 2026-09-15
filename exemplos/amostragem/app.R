# Amostragem — o caminho de uma pesquisa, nas populações com a verdade
# conhecida: planejar o tamanho (média, proporção, estratificada com Neyman,
# conglomerados pelo ICC), sortear (AAS, estratificada pelo plano, PPS, dois
# estágios), estimar com o erro do desenho, simular os desenhos lado a lado, e
# declarar o desenho de uma pesquisa já coletada.
#
# Em desenvolvimento (fora de pacote instalado), núcleo e coleções entram por
# pkgload. Depois de instalados, isto vira duas linhas:
#   library(trama)
#   tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.sampling")))
# Rodável de qualquer lugar: `Rscript exemplos/amostragem/app.R` da raiz do
# repo, ou `Rscript app.R` daqui — os caminhos são relativos a ESTE arquivo.
setwd(local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[[1]]))) else normalizePath(getwd())
}))

pkgload::load_all("../..", quiet = TRUE, attach = TRUE)
pkgload::load_all("../../collections/trama.data", quiet = TRUE, attach = FALSE)
# A ordem importa e não é estilo: a `view` declara portas `data/table`, e a
# `sampling` declara portas `data/table` e `view/plot`. O registro recusa porta
# com tipo desconhecido, então carregar fora de ordem falha dizendo qual tipo
# faltou.
pkgload::load_all("../../collections/trama.view", quiet = TRUE, attach = FALSE)
pkgload::load_all("../../collections/trama.sampling", quiet = TRUE, attach = FALSE)

tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.sampling")))
