#' Random forest do scikit-learn
#'
#' Ajusta `RandomForestClassifier`/`RandomForestRegressor` pelo Python e devolve
#' um `models/fit`: prever, avaliar, ROC e importância são os blocos da
#' `trama.models`, sem nada específico de Python.
#' @param dados data.frame de treino.
#' @param resposta coluna a prever.
#' @param preditores colunas preditoras; vazio usa as numéricas exceto a resposta.
#' @param tarefa `"auto"`, `"regressao"` ou `"classificacao"`.
#' @param trees número de árvores (`n_estimators`).
#' @param seed semente (`random_state`).
#' @return Objeto de classe `c("tr_py_fit", "tr_models_fit")`.
#' @export
tr_py_forest <- function(dados, resposta = "", preditores = "", tarefa = "auto",
                         trees = 200L, seed = 42L) {
  dados <- as.data.frame(dados, check.names = FALSE)
  if (!nzchar(resposta) || !resposta %in% names(dados)) {
    rlang::abort("Escolha a coluna `resposta`.", class = "tr_models_error_bad_input")
  }
  preditores <- .tr_py_preditores(dados, resposta, preditores)
  y <- dados[[resposta]]
  if (tarefa == "auto") tarefa <- if (is.numeric(y)) "regressao" else "classificacao"
  niveis <- NULL
  if (tarefa == "classificacao") {
    niveis <- levels(factor(y))
    y <- as.character(y)
  }
  X <- .tr_py_X(dados, preditores)
  est <- .tr_py_mod()$ajustar(X, y, tarefa, trees, seed)
  structure(list(estimador = est, tarefa = tarefa, resposta = resposta,
                 preditores = preditores, niveis = niveis, n = nrow(dados),
                 trees = as.integer(trees), seed = as.integer(seed),
                 dados = dados[, c(resposta, preditores), drop = FALSE],
                 sklearn = reticulate::import("sklearn")$`__version__`),
            class = c("tr_py_fit", "tr_models_fit"))
}

.tr_py_preditores <- function(dados, resposta, preditores) {
  p <- trimws(strsplit(paste(preditores, collapse = ","), ",")[[1]])
  p <- p[nzchar(p)]
  if (length(p)) return(p)
  num <- names(dados)[vapply(dados, is.numeric, logical(1))]
  setdiff(num, resposta)
}

.tr_py_rotulo <- function(x) {
  paste0("Random forest (sklearn) \u{B7} ",
         if (x$tarefa == "regressao") "regress\u{E3}o" else "classifica\u{E7}\u{E3}o")
}

# ---- Contrato da trama.models ----------------------------------------------------

#' @export
tr_models_info.tr_py_fit <- function(x) {
  list(tarefa = x$tarefa, resposta = x$resposta, preditores = x$preditores,
       niveis = x$niveis, n = x$n, rotulo = .tr_py_rotulo(x), familia = NULL)
}

#' @export
tr_models_predict_raw.tr_py_fit <- function(x, novos, ...) {
  .tr_py_formato(x, .tr_py_mod()$prever(.tr_py_estimador(x), .tr_py_X(novos, x$preditores)))
}

#' Cruzada = `cross_val_predict` do sklearn, 5 folds estratificados, mesma semente.
#' @export
tr_models_predict_cv.tr_py_fit <- function(x, validacao = "resubstitui\u{E7}\u{E3}o") {
  if (validacao != "cruzada") return(tr_models_predict_raw.tr_py_fit(x, x$dados))
  y <- x$dados[[x$resposta]]
  if (x$tarefa == "classificacao") y <- as.character(y)
  k <- if (x$tarefa == "classificacao") min(5L, min(table(y))) else 5L
  p <- .tr_py_mod()$prever_cv(.tr_py_X(x$dados, x$preditores), y, x$tarefa, x$trees, x$seed, k)
  .tr_py_formato(x, p)
}

#' @export
tr_models_stats.tr_py_fit <- function(x) {
  tibble::tibble(modelo = .tr_py_rotulo(x), motor = paste0("scikit-learn ", x$sklearn),
                 n = x$n, trees = x$trees, seed = x$seed)
}

#' @export
tr_models_importance.tr_py_fit <- function(x) {
  imp <- as.numeric(.tr_py_mod()$importancia(.tr_py_estimador(x)))
  tibble::tibble(termo = x$preditores, importancia = imp, medida = "impureza (MDI)")
}

#' @export
tr_models_as_table.tr_py_fit <- function(x) {
  cbind(tr_models_stats.tr_py_fit(x), tibble::tibble(preditores = paste(x$preditores, collapse = ", ")))
}

#' @export
tr_models_card.tr_py_fit <- function(x, ctx) {
  tabela <- Filter(function(t) identical(t$id, "data/table"), trama.data::trama_collection()$types)[[1L]]
  tabela$preview(tr_models_as_table.tr_py_fit(x), ctx)
}

#' O estimador vira bytes do pickle; o resto segue no RDS da models.
#' @export
tr_models_serialize.tr_py_fit <- function(x) {
  if (!is.raw(x$estimador)) x$estimador <- as.raw(.tr_py_mod()$gravar(x$estimador))
  x
}

#' Não reabre: os bytes ficam até o primeiro uso (`.tr_py_estimador`).
#' @export
tr_models_unserialize.tr_py_fit <- function(x) x
