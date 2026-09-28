# A ponte. Tudo que fala com Python passa por aqui; o resto da coleção só vê R.
#
# O núcleo não sabe que existe Python: o bloco é uma função R, e o modelo viaja
# no tipo `models/fit` da models. O único cuidado é o do xgboost na ml — o
# estimador é um objeto vivo do interpretador e morre no `saveRDS`. Então o
# `tr_models_serialize` troca-o pelos bytes do `pickle`, e o
# `tr_models_unserialize` NÃO reabre na hora: guarda os bytes e reabre no
# primeiro uso (`.tr_py_estimador`). Assim um daemon do `mirai` que só repassa
# o modelo nem chega a subir o Python.

.tr_py <- new.env(parent = emptyenv())

#' O módulo `trama_sklearn`, importado uma vez por processo.
#' @noRd
.tr_py_mod <- function() {
  if (is.null(.tr_py$mod)) {
    # Ambiente efêmero e cacheado do reticulate (uv): não toca o Python do
    # sistema. Quem já tem um Python configurado (RETICULATE_PYTHON) usa o dele.
    reticulate::py_require(c("scikit-learn", "pandas"))
    .tr_py$mod <- reticulate::import_from_path(
      "trama_sklearn", system.file("python", package = "trama.python"), convert = TRUE)
  }
  .tr_py$mod
}

#' O estimador vivo; reabre do pickle se o modelo veio do store.
#' @noRd
.tr_py_estimador <- function(x) {
  if (is.raw(x$estimador)) .tr_py_mod()$ler(as.integer(x$estimador)) else x$estimador
}

#' data.frame R -> pandas só com os preditores, na ordem do ajuste.
#' @noRd
.tr_py_X <- function(dados, preditores) {
  .tr_py_mod()  # sobe o Python com pandas antes de converter
  faltam <- setdiff(preditores, names(dados))
  if (length(faltam)) {
    rlang::abort(sprintf("Faltam colunas para prever: %s.", paste(faltam, collapse = ", ")),
                 class = "tr_models_error_bad_input")
  }
  reticulate::r_to_py(as.data.frame(dados, check.names = FALSE)[, preditores, drop = FALSE])
}

#' Resultado do Python -> o formato `list(previsto, prob, extra)` do contrato.
#'
#' O sklearn ordena `classes_`; o contrato quer as colunas de `prob` na ordem de
#' `info$niveis`. Reordena por nome, nunca por posição.
#' @noRd
.tr_py_formato <- function(x, p) {
  if (x$tarefa == "regressao") return(list(previsto = as.numeric(p$previsto), prob = NULL, extra = NULL))
  prob <- as.matrix(p$prob)
  colnames(prob) <- p$classes
  list(previsto = factor(as.character(p$previsto), levels = x$niveis),
       prob = prob[, x$niveis, drop = FALSE], extra = NULL)
}
