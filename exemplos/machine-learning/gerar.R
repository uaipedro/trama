# Regenera os documentos dos exemplos. Execute da raiz do repositório.
pkgload::load_all(".", quiet = TRUE)
pkgload::load_all("collections/trama.data", quiet = TRUE, attach = FALSE)
pkgload::load_all("collections/trama.view", quiet = TRUE, attach = FALSE)
pkgload::load_all("collections/trama.ml", quiet = TRUE, attach = FALSE)
reg <- trama::tr_registry()
trama::tr_use("trama.data", registry = reg)
trama::tr_use("trama.view", registry = reg)
trama::tr_use("trama.ml", registry = reg)

comparar <- function(nome, resposta, preditores = "") {
  f <- trama::tr_flow(reg) |>
    trama::tr_add("dados", "ml/example", nome = nome, position = c(0, 300)) |>
    trama::tr_add("divisao", "ml/split", resposta = resposta, from = "dados", position = c(320, 300))
  metodos <- c("linear", "cart", "figs", "forest", "svm", "xgboost")
  for (i in seq_along(metodos)) {
    id <- metodos[[i]]
    f <- trama::tr_add(f, id, paste0("ml/", id), resposta = resposta, preditores = preditores,
                      from = "divisao:treino", position = c(680, (i - 1) * 320))
    f <- trama::tr_add(f, paste0(id, "_prever"), "ml/predict",
                      from = c(id, "divisao:teste"), position = c(1040, (i - 1) * 320))
    f <- trama::tr_add(f, paste0(id, "_avaliar"), "ml/evaluate", resposta = resposta,
                      from = paste0(id, "_prever"), position = c(1400, (i - 1) * 320))
  }
  f
}
# Só os dois métodos interpretáveis (uma árvore vs. soma de poucas árvores),
# com as regras expostas — o comparativo de `comparar()` acima é sobre
# métrica; este é sobre LEITURA do modelo, por isso cada método ganha
# `ml/rules` além de prever/avaliar.
cart_vs_figs <- function() {
  f <- trama::tr_flow(reg) |>
    trama::tr_add("dados", "ml/example", nome = "iris_binaria", position = c(0, 150)) |>
    trama::tr_add("divisao", "ml/split", resposta = "Species", from = "dados", position = c(320, 150))
  metodos <- c("cart", "figs")
  for (i in seq_along(metodos)) {
    id <- metodos[[i]]
    f <- trama::tr_add(f, id, paste0("ml/", id), resposta = "Species",
                      from = "divisao:treino", position = c(680, (i - 1) * 400))
    f <- trama::tr_add(f, paste0(id, "_prever"), "ml/predict",
                      from = c(id, "divisao:teste"), position = c(1040, (i - 1) * 400))
    f <- trama::tr_add(f, paste0(id, "_avaliar"), "ml/evaluate", resposta = "Species",
                      from = paste0(id, "_prever"), position = c(1400, (i - 1) * 400))
    f <- trama::tr_add(f, paste0(id, "_regras"), "ml/rules",
                      from = id, position = c(1400, (i - 1) * 400 + 160))
  }
  f
}

destino <- "exemplos/machine-learning/flows"
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
trama::tr_doc_write(trama::tr_flow_doc(comparar("iris_binaria", "Species")),
                    file.path(destino, "main.json"))
trama::tr_doc_write(trama::tr_flow_doc(comparar("mtcars", "mpg", "wt, hp, disp")),
                    file.path(destino, "regressao.json"))
trama::tr_doc_write(trama::tr_flow_doc(cart_vs_figs()),
                    file.path(destino, "cart-vs-figs.json"))
