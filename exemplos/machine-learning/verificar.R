# Verificação dos documentos salvos, usando os motores reais.
# Execute da raiz do repositório, com os motores instalados.
#
# Abre cada fluxo como o editor abre (ler, migrar, validar), roda tudo e
# imprime as métricas de cada `models/evaluate` — os fluxos foram gravados com
# os blocos antigos da ml, e a migração da trama.models os traz para os de lá.
# Os nós são os que EXISTEM em cada documento, e não uma lista fixa: `main` foi
# editado à mão e tem só CART e FIGS.
# As métricas podem ter NA legítimo (precisão de classe nunca prevista, main
# 095f4d9), então a checagem de finitude é só sobre as que existem.
pkgload::load_all(".", quiet = TRUE)
for (p in c("trama.data", "trama.view", "trama.models", "trama.ml"))
  pkgload::load_all(file.path("collections", p), quiet = TRUE, attach = FALSE)
reg <- trama::tr_registry()
for (p in c("trama.data", "trama.view", "trama.models", "trama.ml")) trama::tr_use(p, registry = reg)
# `main` lê heart.csv por caminho relativo à pasta do exemplo.
setwd("exemplos/machine-learning")
for (arquivo in c("main", "regressao", "cart-vs-figs")) {
  doc <- trama::tr_doc_migrate(trama::tr_doc_read(file.path("flows", paste0(arquivo, ".json"))), reg)
  problemas <- trama::tr_doc_validate(doc, reg)
  stopifnot(length(problemas) == 0L)
  store <- trama::tr_store(tempfile())
  res <- trama::tr_run(doc, registry = reg, store = store)
  if (length(res$skipped)) stop(arquivo, ": sem valor em ", paste(res$skipped, collapse = ", "))
  tipos <- vapply(doc$nodes, `[[`, "", "type")
  for (id in names(tipos)[tipos == "models/evaluate"]) {
    tab <- trama::tr_value(doc, id, registry = reg, store = store)
    v <- tab$valor[!is.na(tab$valor)]
    stopifnot(nrow(tab) >= 3L, length(v) > 0, all(is.finite(v)), all(tab$n > 0))
    cat(arquivo, id, paste(tab$metrica, signif(tab$valor, 4), collapse = "; "), "\n")
  }
  # As regras de CART e FIGS: o ponto de cart-vs-figs é que saiam não-vazias.
  for (id in names(tipos)[tipos == "ml/rules"]) {
    regras <- trama::tr_value(doc, id, registry = reg, store = store)
    stopifnot(nrow(regras) > 0)
    cat(arquivo, id, nrow(regras), "regras\n")
  }
}
