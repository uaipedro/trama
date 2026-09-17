# Regenera os documentos dos exemplos. Execute da raiz do repositório.
pkgload::load_all(".", quiet = TRUE)
pkgload::load_all("collections/trama.data", quiet = TRUE, attach = FALSE)
pkgload::load_all("collections/trama.ml", quiet = TRUE, attach = FALSE)
reg <- trama::tr_registry()
trama::tr_use("trama.data", registry = reg)
trama::tr_use("trama.ml", registry = reg)

comparar <- function(nome, alvo, cols = "") {
  f <- trama::tr_flow(reg) |>
    trama::tr_add("dados", "ml/example", nome = nome, position = c(0, 300)) |>
    trama::tr_add("divisao", "ml/split", alvo = alvo, from = "dados", position = c(320, 300))
  metodos <- c("linear", "cart", "figs", "forest", "svm", "xgboost")
  for (i in seq_along(metodos)) {
    id <- metodos[[i]]
    f <- trama::tr_add(f, id, paste0("ml/", id), alvo = alvo, cols = cols,
                      from = "divisao:treino", position = c(680, (i - 1) * 320))
    f <- trama::tr_add(f, paste0(id, "_prever"), "ml/predict",
                      from = c(id, "divisao:teste"), position = c(1040, (i - 1) * 320))
    f <- trama::tr_add(f, paste0(id, "_avaliar"), "ml/evaluate", alvo = alvo,
                      from = paste0(id, "_prever"), position = c(1400, (i - 1) * 320))
  }
  f
}
destino <- "exemplos/machine-learning/flows"
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
trama::tr_doc_write(trama::tr_flow_doc(comparar("iris_binaria", "Species")),
                    file.path(destino, "main.json"))
trama::tr_doc_write(trama::tr_flow_doc(comparar("mtcars", "mpg", "wt, hp, disp")),
                    file.path(destino, "regressao.json"))
