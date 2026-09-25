# Pressupostos e referências: testes não paramétricos de tendência, mudança e
# sazonalidade.

.tr_series_docs_tendencia <- function() {
  P <- .tr_series_P; I <- .tr_series_impl; R <- trama::tr_ref
  L <- .tr_series_livros()
  indep <- function(efeito) P(
    sprintf("Sob H0 as observações são **independentes** (sem autocorrelação). %s", efeito),
    verificar = c("series/acf", "series/ljung_box"),
    se_falhar = "Olhe a `series/acf` da série sem a tendência (resto do `series/stl` via `series/component`). Versões para dado autocorrelacionado (Mann-Kendall modificado, pré-branqueamento) ainda sem bloco no trama: leia o p-valor como otimista.")
  continua <- P("A variável é **contínua**: empates são raros. Os empatados saem da conta (ou corrigem a variância), e com muitos o teste perde informação.")
  monotona <- P(
    "A tendência procurada é **monótona** (um sentido só). Uma série que sobe e depois desce pode sair sem tendência nenhuma.",
    verificar = "series/plot",
    se_falhar = "Separe os trechos (`series/window`) ou procure o ponto de mudança com o `series/pettitt`.")
  hamed_rao <- R(autores = c("Hamed, K. H.", "Rao, A. R."), ano = 1998,
                 titulo = "A modified Mann-Kendall trend test for autocorrelated data",
                 fonte = "Journal of Hydrology, 204(1-4), 182-196",
                 doi = "10.1016/S0022-1694(97)00125-X", papel = "complementar")
  list(
    "series/mann_kendall" = list(
      pressupostos = list(
        indep("Autocorrelação positiva, comum em série ambiental, faz o teste rejeitar bem mais que o nível nominal."),
        monotona, continua,
        P("Pelo menos **10** observações para a aproximação normal de S.")),
      referencias = list(
        R(autores = "Mann, H. B.", ano = 1945, titulo = "Nonparametric tests against trend",
          fonte = "Econometrica, 13(3), 245-259", doi = "10.2307/1907187"),
        L$morettin, hamed_rao,
        I("trama.series", "tr_series_mann_kendall", "Cálculo próprio: S sobre todos os pares, variância com correção de empates, Z com correção de continuidade e p-valor normal bilateral."))),

    "series/cox_stuart" = list(
      pressupostos = list(
        indep("Os pares são tratados como independentes; com autocorrelação o teste rejeita demais."),
        monotona, continua,
        P("Sobram **pelo menos 6 pares** depois dos empates (o bloco pede 16 observações); com menos, nem o resultado mais extremo rejeita a 5%.")),
      referencias = list(
        R(autores = c("Cox, D. R.", "Stuart, A."), ano = 1955,
          titulo = "Some quick sign tests for trend in location and dispersion",
          fonte = "Biometrika, 42(1-2), 80-95", doi = "10.1093/biomet/42.1-2.80"),
        L$morettin,
        I("trama.series", "tr_series_cox_stuart", "Cálculo próprio: pares em terços (original) ou metades, empates descartados; `stats::binom.test` com menos de 20 pares, aproximação normal a partir de 20."))),

    "series/runs" = list(
      pressupostos = list(
        P("O que se testa é a **aleatoriedade** da ordem: rejeitar não diz qual é a causa (tendência, mudança de nível, ciclo, agrupamento).",
          verificar = "series/plot",
          se_falhar = "Para afirmar tendência, `series/mann_kendall`; para mudança de nível, `series/pettitt`."),
        continua,
        P("Pelo menos **20 observações de cada lado** da mediana, para a aproximação normal do número de sequências (o bloco pede 40)."),
        P("É o teste de **menor poder** entre os de tendência: um \"não rejeita\" sozinho é evidência fraca.",
          se_falhar = "Rode ao lado o `series/mann_kendall`.")),
      referencias = list(
        R(autores = c("Wald, A.", "Wolfowitz, J."), ano = 1940,
          titulo = "On a test whether two samples are from the same population",
          fonte = "The Annals of Mathematical Statistics, 11(2), 147-162",
          doi = "10.1214/aoms/1177731909"),
        L$morettin, L$siegel,
        I("trama.series", "tr_series_runs", "Cálculo próprio: símbolos acima/abaixo da mediana (iguais à mediana fora da conta), Z pela média e variância do número de sequências, p-valor normal bilateral, sem correção de continuidade."))),

    "series/pettitt" = list(
      pressupostos = list(
        indep("Autocorrelação positiva imita ponto de mudança e faz o teste rejeitar demais."),
        P("Há **no máximo um** ponto de mudança, de **locação** (a distribuição antes e depois difere no nível). Várias mudanças, ou uma tendência gradual, também rejeitam, e a posição apontada não é então uma quebra.",
          verificar = c("series/plot", "series/mann_kendall"),
          se_falhar = "Analise os trechos em separado (`series/window`)."),
        continua,
        P("O p-valor é **aproximado** (e conservador); o bloco pede pelo menos 11 observações.")),
      referencias = list(
        R(autores = "Pettitt, A. N.", ano = 1979,
          titulo = "A non-parametric approach to the change-point problem",
          fonte = "Journal of the Royal Statistical Society. Series C (Applied Statistics), 28(2), 126-135",
          doi = "10.2307/2346729"),
        I("trama.series", "tr_series_pettitt", "Cálculo próprio: U_t acumulado de Mann-Whitney, K = max|U_t|, p ≈ 2·exp(−6K²/(n³ + n²)) limitado a 1."))),

    "series/kruskal_wallis" = list(
      pressupostos = list(
        P("A série **não tem tendência**: com tendência os postos altos ficam todos nos anos finais, espalhados por todas as estações, e a sazonalidade some do teste.",
          verificar = c("series/plot", "series/mann_kendall"),
          se_falhar = "Tire a tendência antes com `series/diff` (o log de `series/transform` não muda nada: o teste é de postos)."),
        indep("A autocorrelação dentro de cada estação (o janeiro de um ano parecido com o do seguinte) torna o p-valor otimista."),
        P("A sazonalidade é **determinística** (o mesmo padrão em todo ciclo) e o **ciclo declarado** é o verdadeiro.",
          verificar = c("series/seasonal_plot", "series/subseries", "series/fisher"),
          se_falhar = "Sazonalidade que muda de ano para ano: diferença sazonal (`series/diff`)."),
        P("Pelo menos **três ciclos**; o H segue o qui-quadrado aproximado com estações − 1 graus de liberdade.")),
      referencias = list(
        R(autores = c("Kruskal, W. H.", "Wallis, W. A."), ano = 1952,
          titulo = "Use of ranks in one-criterion variance analysis",
          fonte = "Journal of the American Statistical Association, 47(260), 583-621",
          doi = "10.1080/01621459.1952.10483441"),
        L$morettin, L$siegel,
        I("stats", "kruskal.test", "Valores da série agrupados pela estação (`cycle()`); p-valor pelo qui-quadrado, com correção de empates."))),

    "series/fisher" = list(
      pressupostos = list(
        P("Sob H0 a série é **ruído branco gaussiano** (independente, normal, variância constante) em torno de uma reta: é daí que sai a distribuição de g.",
          verificar = c("series/acf", "series/ljung_box"),
          se_falhar = "Autocorrelação sem ciclo (um AR, por exemplo) já concentra potência nas frequências baixas e faz o teste rejeitar: leia o período do pico."),
        P("A tendência é **no máximo linear** (o periodograma remove uma reta). Tendência curva ou estrutura de baixa frequência vira um pico no período da série inteira.",
          verificar = "series/plot",
          se_falhar = "Diferencie antes (`series/diff`) e desconfie de pico que não se repete (a `nota` avisa)."),
        P("A periodicidade cai **na grade de Fourier** (período N/j) e há **uma** dominante: o teste olha só o maior pico.",
          verificar = c("series/acf", "series/seasonal_plot"))),
      referencias = list(
        R(autores = "Fisher, R. A.", ano = 1929, titulo = "Tests of significance in harmonic analysis",
          fonte = "Proceedings of the Royal Society of London. Series A, 125(796), 54-59",
          doi = "10.1098/rspa.1929.0151"),
        L$morettin,
        I("trama.series", "tr_series_fisher", "Cálculo próprio sobre `stats::spec.pgram(taper = 0, detrend = TRUE, fast = FALSE)`: g = maior ordenada / soma, p pelo primeiro termo da série exata de Fisher (conservador), zα publicado como crítico a 5%.")))
  )
}
