# Execute da raiz: Rscript exemplos/machine-learning/app.R
args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(args)) setwd(dirname(normalizePath(sub("^--file=", "", args[[1]]))))
pkgload::load_all("../..", quiet = TRUE)
pkgload::load_all("../../collections/trama.data", quiet = TRUE, attach = FALSE)
pkgload::load_all("../../collections/trama.ml", quiet = TRUE, attach = FALSE)
trama::tr_app(trama::tr_project(".", collections = c("trama.data", "trama.ml")))
