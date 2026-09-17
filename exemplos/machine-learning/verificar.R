# Verificação dos documentos salvos, usando os motores reais.
# Execute da raiz do repositório, com os motores instalados.
pkgload::load_all(".", quiet = TRUE)
pkgload::load_all("collections/trama.data", quiet = TRUE, attach = FALSE)
pkgload::load_all("collections/trama.ml", quiet = TRUE, attach = FALSE)
reg <- trama::tr_registry()
trama::tr_use("trama.data", registry = reg)
trama::tr_use("trama.ml", registry = reg)
for (arquivo in c("main", "regressao")) {
  doc <- trama::tr_doc_read(paste0("exemplos/machine-learning/flows/", arquivo, ".json"))
  store <- trama::tr_store(tempfile())
  for (metodo in c("linear", "cart", "figs", "forest", "svm", "xgboost")) {
    tab <- trama::tr_value(doc, paste0(metodo, "_avaliar"), registry = reg, store = store)
    stopifnot(nrow(tab) == 3L, all(is.finite(tab$valor)), all(tab$n > 0))
    cat(arquivo, metodo, paste(tab$metrica, signif(tab$valor, 4), collapse = "; "), "\n")
  }
}

# cart-vs-figs.json soma `ml/rules` aos dois métodos interpretáveis: o ponto
# não é métrica (já coberta acima em "main"), é confirmar que a regra sai
# não-vazia dos dois motores.
doc <- trama::tr_doc_read("exemplos/machine-learning/flows/cart-vs-figs.json")
store <- trama::tr_store(tempfile())
for (metodo in c("cart", "figs")) {
  tab <- trama::tr_value(doc, paste0(metodo, "_avaliar"), registry = reg, store = store)
  stopifnot(nrow(tab) == 3L, all(is.finite(tab$valor)), all(tab$n > 0))
  regras <- trama::tr_value(doc, paste0(metodo, "_regras"), registry = reg, store = store)
  stopifnot(nrow(regras) > 0)
  cat("cart-vs-figs", metodo, paste(tab$metrica, signif(tab$valor, 4), collapse = "; "),
      "| regras:", nrow(regras), "linhas\n")
}
