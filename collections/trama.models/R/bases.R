#' Bases para regressão e modelos lineares generalizados. Fonte, dimensões e
#' licença conferidas no Rd e no DESCRIPTION de cada pacote (30/09/2026).
#' @noRd
.tr_models_bases <- function() {
  b <- trama.data::tr_base_publica
  list(
    b("carData", "Prestige", "Prestígio de 102 ocupações canadenses",
      temas = c("regressão"), n = 102, variaveis = 6,
      fonte = "Census of Canada (1971); em Fox, Applied Regression Analysis and GLMs",
      licenca = "GPL (>= 2)"),
    b("carData", "Salaries", "Salários de professores nos EUA, 2008-09",
      temas = c("regressão", "anova"), n = 397, variaveis = 6,
      fonte = "Fox & Weisberg, An R Companion to Applied Regression", licenca = "GPL (>= 2)"),
    b("carData", "Davis", "Altura e peso medidos e autorrelatados",
      temas = c("regressão", "outliers"), n = 200, variaveis = 5,
      fonte = "C. Davis, York University (comunicação pessoal a J. Fox)", licenca = "GPL (>= 2)"),
    b("carData", "TitanicSurvival", "Sobrevivência dos passageiros do Titanic",
      temas = c("logística", "classificação"), n = 1309, variaveis = 4,
      fonte = "Conjunto titanic3, Vanderbilt Biostatistics", licenca = "GPL (>= 2)"),
    b("MASS", "Boston", "Valor de imóveis em Boston",
      temas = c("regressão"), n = 506, variaveis = 14,
      fonte = "Harrison & Rubinfeld (1978), J. Environ. Economics and Management 5, 81-102",
      licenca = "GPL-2 | GPL-3"),
    b("MASS", "cats", "Peso do corpo e do coração de gatos",
      temas = c("regressão", "ancova"), n = 144, variaveis = 3,
      fonte = "Fisher (1947), Biometrics 3, 65-68", licenca = "GPL-2 | GPL-3"),
    b("MASS", "birthwt", "Fatores de risco para baixo peso ao nascer",
      temas = c("logística"), n = 189, variaveis = 10,
      fonte = "Hosmer & Lemeshow (1989), Applied Logistic Regression", licenca = "GPL-2 | GPL-3"),
    b("MASS", "whiteside", "Consumo de gás antes e depois de isolar a casa",
      temas = c("regressão", "ancova"), n = 56, variaveis = 3,
      fonte = "Hand et al. (1993), A Handbook of Small Data Sets", licenca = "GPL-2 | GPL-3"),
    b("faraway", "gala", "Diversidade de espécies nas Ilhas Galápagos",
      temas = c("regressão", "contagem"), n = 30, variaveis = 7,
      fonte = "Johnson & Raven (1973), Science 179, 893-895", licenca = "GPL"),
    b("faraway", "pima", "Diabetes em mulheres Pima",
      temas = c("logística", "faltantes"), n = 768, variaveis = 9,
      fonte = "UCI Machine Learning Repository", licenca = "GPL")
  )
}
