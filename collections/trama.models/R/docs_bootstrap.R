.tr_models_docs_bootstrap <- function() {
  R <- trama::tr_ref
  list("models/bootstrap" = list(
    pressupostos = list(trama::tr_pressuposto(
      "As linhas são unidades independentes (dentro de cada estrato): o bootstrap de casos não serve para medidas repetidas, parcelas subdivididas ou séries.")),
    referencias = list(
      .tr_models_impl("boot", "boot"), .tr_models_impl("boot", "boot.ci"),
      R(autores = c("Davison, A. C.", "Hinkley, D. V."), ano = 1997,
        titulo = "Bootstrap Methods and Their Application",
        fonte = "Cambridge University Press", papel = "livro-texto"),
      R(autores = c("Efron, B.", "Tibshirani, R. J."), ano = 1993,
        titulo = "An Introduction to the Bootstrap",
        fonte = "Chapman & Hall", papel = "livro-texto"))),
    "models/residuals" = list(
      referencias = list(
        R(autores = c("Dunn, P. K.", "Smyth, G. K."), ano = 1996,
          titulo = "Randomized quantile residuals",
          fonte = "Journal of Computational and Graphical Statistics, 5(3), 236-244",
          doi = "10.1080/10618600.1996.10474708"))))
}
