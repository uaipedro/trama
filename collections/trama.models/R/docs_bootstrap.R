.tr_models_docs_bootstrap <- function() {
  R <- trama::tr_ref
  list("models/bootstrap" = list(
    pressupostos = list(trama::tr_pressuposto(
      "As linhas são reamostradas como unidades independentes dentro de cada estrato; o ajuste e a especificação do modelo permanecem fixos.",
      verificar = "models/anova_table")),
    referencias = list(
      .tr_models_impl("boot", "boot"), .tr_models_impl("boot", "boot.ci"),
      R(autores = c("Davison, A. C.", "Hinkley, D. V."), ano = 1997,
        titulo = "Bootstrap Methods and Their Application",
        fonte = "Cambridge University Press", papel = "livro-texto"))))
}
