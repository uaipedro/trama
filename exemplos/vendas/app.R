# Editor de fluxo do trama, um arquivo só.
#
# Em desenvolvimento (fora de pacote instalado), núcleo e coleção entram por
# pkgload. Depois de instalados, isto vira duas linhas:
#   library(trama); tr_app(tr_project(".", collections = "trama.data"))
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

tr_app(tr_project(".", collections = "trama.data"))

# Variante com pool `mirai` (não bloqueia o processo do Shiny; exige `mirai`
# instalado). `setup` é necessário em desenvolvimento — sem pacote instalado,
# `trama:::` dentro do daemon não resolveria nada — e é o que permite validar
# o pool no ciclo rápido, sem `R CMD INSTALL`.
# project <- tr_project(".", collections = "trama.data")
# core <- normalizePath("../.."); coll <- normalizePath("../../collections/trama.data")
# executor <- tr_executor_pool(2L, registry = project$registry, setup = bquote({
#   pkgload::load_all(.(core), quiet = TRUE, attach = FALSE)
#   pkgload::load_all(.(coll), quiet = TRUE, attach = FALSE, export_all = FALSE)
# }))
# tr_app(project, executor = executor)
