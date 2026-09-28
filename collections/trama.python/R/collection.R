#' Coleção Python (prova de conceito).
#'
#' Carregue `trama.data`, `trama.view` e `trama.models` antes: o ajuste sai em
#' `models/fit`, e os leitores são os da models.
#' @return Uma declaração `tr_collection`.
#' @export
trama_collection <- function() {
  trama::tr_collection("python", version = "0.0.1", label = "Python (sklearn)",
    categories = list(trama::tr_category("py_ajuste", "scikit-learn", role = "ajuste")),
    nodes = list(
      trama::tr_node("python/forest", tr_py_forest, label = "Random forest (sklearn)",
        description = "Random forest do scikit-learn; sai como models/fit e liga em Prever, Avaliar e ROC.",
        category = "py_ajuste", icon = trama::tr_icon("trees"),
        inputs = list(dados = "data/table"), outputs = list(out = "models/fit"),
        params = list(
          resposta = trama::tr_param_col("", label = "Resposta", role = "qualquer", example = "Species"),
          preditores = trama::tr_param_col("", label = "Preditores", role = "qualquer", multi = TRUE,
                                           example = "Sepal.Length, Petal.Length"),
          tarefa = trama::tr_param_enum("auto", c("auto", "regressao", "classificacao"), label = "Tarefa"),
          trees = trama::tr_param_int(200L, min = 1L, label = "\u{C1}rvores"),
          seed = trama::tr_param_int(42L, min = 0L, label = "Semente")),
        # O código Python não está no fecho da função R, então o hash do nó não
        # o vê. A versão do sklearn e o próprio .py entram pela impressão digital.
        pure = FALSE,
        fingerprint = function(params) {
          py <- system.file("python", "trama_sklearn.py", package = "trama.python")
          .tr_py_mod()
          list(sklearn = reticulate::import("sklearn")$`__version__`,
               codigo = unname(tools::md5sum(py)))
        })))
}
