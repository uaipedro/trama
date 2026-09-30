#' Declara uma base pública para o catálogo de bases, carregada pelo bloco
#' `data/public`.
#'
#' É o atalho que as outras coleções usam: todas pedem o mesmo bloco, e
#' escrever `node`/`params` a cada entrada seria repetir o par
#' `pacote`/`dataset` duas vezes. Os metadados (`fonte`, `n`, `variaveis`,
#' `licenca`) são conferidos na documentação do pacote, não de memória.
#' @param ... Metadados repassados a [trama::tr_dataset()].
#' @export
tr_base_publica <- function(pacote, dataset, titulo, ...) {
  trama::tr_dataset(pacote, dataset, titulo, node = "data/public",
                    params = list(pacote = pacote, dataset = dataset), ...)
}

#' Bases de `datasets` saem pelo `data/example`: é o bloco que já as carrega,
#' e um fluxo antigo e um novo ficam iguais.
#' @noRd
.tr_base_exemplo <- function(dataset, titulo, ...) {
  trama::tr_dataset("datasets", dataset, titulo, node = "data/example",
                    params = list(dataset = dataset), licenca = "Part of R", ...)
}

#' Bases de uso geral que a coleção `data` oferece. Fonte, dimensões e licença
#' conferidas no Rd e no DESCRIPTION de cada pacote (30/09/2026).
#' @noRd
.tr_data_bases <- function() list(
  .tr_base_exemplo("mtcars", "Consumo de 32 carros (Motor Trend, 1974)",
    temas = c("regressão", "didático"), n = 32, variaveis = 11,
    fonte = "Henderson & Velleman (1981), Biometrics 37, 391-411"),
  .tr_base_exemplo("iris", "Medidas de flores de íris (Anderson/Fisher)",
    temas = c("classificação", "multivariada", "didático"), n = 150, variaveis = 5,
    fonte = "Fisher (1936), Annals of Eugenics 7, 179-188"),
  .tr_base_exemplo("airquality", "Qualidade do ar em Nova York, 1973",
    temas = c("faltantes", "regressão"), n = 153, variaveis = 6,
    fonte = "New York State Department of Conservation e National Weather Service"),
  .tr_base_exemplo("ToothGrowth", "Crescimento de dentes de cobaias por vitamina C",
    temas = c("experimentos", "anova", "didático"), n = 60, variaveis = 3,
    fonte = "Crampton (1947), The Journal of Nutrition 33, 491-504"),
  .tr_base_exemplo("PlantGrowth", "Peso de plantas sob dois tratamentos e controle",
    temas = c("experimentos", "anova", "didático"), n = 30, variaveis = 2,
    fonte = "Dobson (1983), An Introduction to Statistical Modelling"),
  .tr_base_exemplo("ChickWeight", "Peso de pintos por dieta ao longo do tempo",
    temas = c("medidas repetidas", "modelos mistos"), n = 578, variaveis = 4,
    fonte = "Crowder & Hand (1990), Analysis of Repeated Measures"),
  .tr_base_exemplo("warpbreaks", "Quebras de fio por tipo de lã e tensão",
    temas = c("experimentos", "contagem"), n = 54, variaveis = 3,
    fonte = "Tippett (1950), Technological Applications of Statistics"),
  tr_base_publica("palmerpenguins", "penguins", "Pinguins da Estação Palmer, Antártida",
    descricao = "Medidas de bico, nadadeira e massa de três espécies; substituto moderno do iris.",
    temas = c("classificação", "didático", "faltantes"), n = 344, variaveis = 8,
    fonte = "Palmer Station Antarctica LTER e K. Gorman (2020), Environmental Data Initiative",
    licenca = "CC0", url = "https://allisonhorst.github.io/palmerpenguins/"),
  tr_base_publica("gapminder", "gapminder", "Expectativa de vida, população e PIB por país, 1952-2007",
    temas = c("painel", "séries", "didático"), n = 1704, variaveis = 6,
    fonte = "Gapminder Foundation", licenca = "CC0",
    url = "https://www.gapminder.org/data/")
)
