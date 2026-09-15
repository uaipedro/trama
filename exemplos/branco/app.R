# Projeto vazio — para montar um fluxo do zero.
#
# Em desenvolvimento (fora de pacote instalado), núcleo e coleção entram por
# pkgload. Depois de instalados, isto vira duas linhas:
#   library(trama)
#   tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.series",
#                                          "trama.multi", "trama.models")))
# Rodável de qualquer lugar: `Rscript exemplos/branco/app.R` da raiz do repo,
# ou `Rscript app.R` daqui. Os caminhos abaixo são relativos a ESTE arquivo, e
# não ao diretório de quem chamou — sem isto o exemplo morria com "Could not
# find a root 'DESCRIPTION' file", nomeando uma pasta acima do repo, que é o
# erro certo apontando pro lugar errado.
setwd(local({
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[[1]]))) else normalizePath(getwd())
}))

pkgload::load_all("../..", quiet = TRUE, attach = TRUE)
pkgload::load_all("../../collections/trama.data", quiet = TRUE, attach = FALSE)
# A ordem importa e não é estilo: a `view` declara portas do tipo `data/table`,
# que pertence à `data`, e o registro recusa porta com tipo desconhecido. Carregar
# ao contrário falha em runtime, dizendo "usa tipo desconhecido: 'data/table'".
pkgload::load_all("../../collections/trama.view", quiet = TRUE, attach = FALSE)
# A `series` vem por último pelo mesmo motivo: as portas dela são `data/table`
# (da `data`) e `view/plot` (da `view`).
pkgload::load_all("../../collections/trama.series", quiet = TRUE, attach = FALSE)
# A `multi` também só depende de `data` e `view`, e por isso entra depois delas.
pkgload::load_all("../../collections/trama.multi", quiet = TRUE, attach = FALSE)
# A `models` idem: portas `data/table` e `view/plot`.
pkgload::load_all("../../collections/trama.models", quiet = TRUE, attach = FALSE)

tr_app(tr_project(".", collections = c("trama.data", "trama.view", "trama.series", "trama.multi",
                                       "trama.models")))
