#' Bases de An Introduction to Statistical Learning (ISLR2). Fonte, dimensões
#' e licença conferidas no Rd e no DESCRIPTION do pacote (30/09/2026).
#' @noRd
.tr_ml_bases <- function() {
  b <- function(...) trama.data::tr_base_publica("ISLR2", ..., licenca = "GPL-2",
                                                 url = "https://www.statlearning.com/")
  list(
    b("Auto", "Consumo e características de 392 carros",
      temas = c("regressão"), n = 392, variaveis = 9, fonte = "StatLib, Carnegie Mellon (ASA 1983)"),
    b("Carseats", "Vendas de cadeirinhas infantis em 400 lojas",
      temas = c("regressão", "árvores"), n = 400, variaveis = 11, fonte = "ISLR2: dados simulados"),
    b("Default", "Inadimplência de cartão de crédito",
      temas = c("classificação", "desbalanceado"), n = 10000, variaveis = 4,
      fonte = "ISLR2: dados simulados"),
    b("Hitters", "Salários de jogadores de beisebol",
      temas = c("regressão", "regularização", "faltantes"), n = 322, variaveis = 20,
      fonte = "StatLib, Carnegie Mellon (ASA 1988)"),
    b("Wage", "Salários na região do Atlântico Médio (EUA)",
      temas = c("regressão", "não linear"), n = 3000, variaveis = 11,
      fonte = "Current Population Survey, março de 2011"),
    b("Smarket", "Retornos diários do S&P 500, 2001-2005",
      temas = c("classificação", "séries"), n = 1250, variaveis = 9, fonte = "Yahoo Finance (via ISLR2)"),
    b("College", "Universidades dos EUA (US News, 1995)",
      temas = c("regressão", "multivariada"), n = 777, variaveis = 18,
      fonte = "StatLib, Carnegie Mellon (ASA 1995)")
  )
}
