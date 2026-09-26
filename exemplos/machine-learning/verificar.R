# Verificação dos documentos salvos, usando os motores reais.
# Execute da raiz do repositório, com os motores instalados.
#
# Os nós de avaliação são descobertos pelo sufixo `_avaliar`, e as regras pelo
# sufixo `_regras`: o script não precisa mudar quando um fluxo ganha ou perde
# um método. As métricas podem ter NA legítimo (precisão de classe nunca
# prevista), então a checagem é só sobre as que existem.
pkgload::load_all(".", quiet = TRUE)
pkgload::load_all("collections/trama.data", quiet = TRUE, attach = FALSE)
pkgload::load_all("collections/trama.view", quiet = TRUE, attach = FALSE)
pkgload::load_all("collections/trama.ml", quiet = TRUE, attach = FALSE)
reg <- trama::tr_registry()
trama::tr_use("trama.data", registry = reg)
trama::tr_use("trama.view", registry = reg)
trama::tr_use("trama.ml", registry = reg)

# O `data/read_csv` resolve `heart.csv` relativo à pasta do projeto.
old <- setwd("exemplos/machine-learning")
on.exit(setwd(old), add = TRUE)

for (arquivo in c("main", "regressao", "cart-vs-figs")) {
  doc <- trama::tr_doc_read(file.path("flows", paste0(arquivo, ".json")))
  store <- trama::tr_store(tempfile())
  ids <- names(doc$nodes)
  avaliar <- grep("_avaliar$", ids, value = TRUE)
  stopifnot(length(avaliar) > 0)
  for (id in avaliar) {
    tab <- trama::tr_value(doc, id, registry = reg, store = store)
    v <- tab$valor[!is.na(tab$valor)]
    stopifnot(nrow(tab) > 0, length(v) > 0, all(is.finite(v)))
    cat(arquivo, id, paste(utils::head(paste(tab$metrica, signif(tab$valor, 4)), 3), collapse = "; "), "\n")
  }
  for (id in grep("_regras$", ids, value = TRUE)) {
    regras <- trama::tr_value(doc, id, registry = reg, store = store)
    stopifnot(nrow(regras) > 0)
    cat(arquivo, id, "|", nrow(regras), "linhas de regra\n")
  }
}
