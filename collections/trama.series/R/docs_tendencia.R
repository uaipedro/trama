# Pressupostos e referências: testes não paramétricos de tendência, mudança e
# sazonalidade.

.tr_series_docs_tendencia <- function() {
  P <- .tr_series_P; I <- .tr_series_impl; R <- trama::tr_ref
  L <- .tr_series_livros()
  indep <- function(efeito) P(
    sprintf("Sob H0 as observações são **independentes** (sem autocorrelação). %s", efeito),
    verificar = c("series/acf", "series/ljung_box"),
    se_falhar = "Olhe a `series/acf` da série sem a tendência (resto do `series/stl` via `series/component`). Este teste não tem versão para dado autocorrelacionado: leia o p-valor como otimista e, para tendência, use o `series/mann_kendall` com **Correção** `hamed_rao` ou `pre_branqueamento`.")
  continua <- P("A variável é **contínua**: empates são raros. Os empatados saem da conta (ou corrigem a variância), e com muitos o teste perde informação.")
  monotona <- P(
    "A tendência procurada é **monótona** (um sentido só). Uma série que sobe e depois desce pode sair sem tendência nenhuma.",
    verificar = "series/plot",
    se_falhar = "Separe os trechos (`series/window`) ou procure o ponto de mudança com o `series/pettitt`.")
  hamed_rao <- R(autores = c("Hamed, K. H.", "Rao, A. R."), ano = 1998,
                 titulo = "A modified Mann-Kendall trend test for autocorrelated data",
                 fonte = "Journal of Hydrology, 204(1-4), 182-196",
                 doi = "10.1016/S0022-1694(97)00125-X", papel = "complementar")
  yue <- R(autores = c("Yue, S.", "Pilon, P.", "Phinney, B.", "Cavadias, G."), ano = 2002,
           titulo = "The influence of autocorrelation on the ability to detect trend in hydrological series",
           fonte = "Hydrological Processes, 16(9), 1807-1829", doi = "10.1002/hyp.1095")
  list(
    "series/mann_kendall" = list(
      pressupostos = list(
        P("Com **Correção** `nenhuma`, sob H0 as observações são **independentes** (sem autocorrelação). Autocorrelação positiva, comum em série ambiental, faz o teste rejeitar bem mais que o nível nominal.",
          verificar = c("series/acf", "series/ljung_box"),
          se_falhar = "Troque a **Correção** para `bootstrap_blocos` (p por bootstrap de blocos móveis; o que mais se aproxima do nível), `hamed_rao` (variância corrigida pelas autocorrelações dos postos) ou `pre_branqueamento` (remove o AR(1) antes do teste)."),
        P("Com `hamed_rao`, a dependência está nas **autocorrelações significativas** dos postos da série sem a tendência de Sen; com `pre_branqueamento`, ela é um **AR(1)** e a tendência é **linear** (Sen). Nenhuma das duas devolve o nível nominal: medido sem tendência, AR(1) phi = 0,6, n = 60, rejeitam a 5% em 21% (`hamed_rao`) e 39% (`pre_branqueamento`, pior que os 31% sem correção; Hamed 2009). Com `bootstrap_blocos`, a dependência é de **curto alcance** (cabe em blocos de √n): medido, 5,5% com phi = 0,3 e n = 120, mas 7,7% a 9,0% com phi = 0,6 (n = 120 e 60).",
          verificar = c("series/acf", "series/pacf"),
          se_falhar = "Com autocorrelação forte, modele o erro: `series/regression` com **Erro** = `arma` e o `series/f_tendencia`."),
        monotona, continua,
        P("Pelo menos **10** observações para a aproximação normal de S.")),
      referencias = list(
        R(autores = "Mann, H. B.", ano = 1945, titulo = "Nonparametric tests against trend",
          fonte = "Econometrica, 13(3), 245-259", doi = "10.2307/1907187"),
        L$morettin,
        R(autores = c("Hamed, K. H.", "Rao, A. R."), ano = 1998,
          titulo = "A modified Mann-Kendall trend test for autocorrelated data",
          fonte = "Journal of Hydrology, 204(1-4), 182-196",
          doi = "10.1016/S0022-1694(97)00125-X"),
        yue,
        R(autores = "Hamed, K. H.", ano = 2009,
          titulo = "Enhancing the effectiveness of prewhitening in trend analysis of hydrologic data",
          fonte = "Journal of Hydrology, 368(1-4), 143-155", doi = "10.1016/j.jhydrol.2009.01.040",
          papel = "complementar"),
        R(autores = c("Kundzewicz, Z. W.", "Robson, A. J."), ano = 2004,
          titulo = "Change detection in hydrological records—a review of the methodology",
          fonte = "Hydrological Sciences Journal, 49(1), 7-19", doi = "10.1623/hysj.49.1.7.53993"),
        R(autores = "Künsch, H. R.", ano = 1989,
          titulo = "The jackknife and the bootstrap for general stationary observations",
          fonte = "The Annals of Statistics, 17(3), 1217-1241", doi = "10.1214/aos/1176347265",
          papel = "complementar"),
        I("trama.series", "tr_series_mann_kendall", "Cálculo próprio: S sobre todos os pares, variância com correção de empates, Z com correção de continuidade e p-valor normal bilateral. `hamed_rao` e `pre_branqueamento` seguem `modifiedmk::mmkh` e `modifiedmk::tfpwmk` (conferidos a 1e-8): autocorrelações dos postos em todas as defasagens, só as significativas a 5%; r1 aplicado sempre. Conferido no texto de Yue et al. (2002, p. 1822-1823, passos 1 a 4): o AR(1) é removido SEMPRE, sem condição de significância; o teste do r1 (eq. B.1, 10% bilateral) só escolhe as estações da aplicação (p. 1825). Diferença documentada: o r1 do artigo é a eq. 14a, com 1/(n − k) no numerador e 1/n no denominador, isto é, n/(n − 1) vezes o r1 do `stats::acf` que o `modifiedmk::tfpwmk` e este bloco usam (com n = 60, 1,7% maior); segue-se o `modifiedmk`, que é o oráculo. `bootstrap_blocos`: blocos móveis de round(√n), inícios sorteados com reposição, 1999 reamostras, p = (1 + #{|S*| ≥ |S|})/(B + 1), semente do nó; conferido contra o mesmo bootstrap escrito à mão com a mesma semente (igual exatamente)."))),

    "series/cox_stuart" = list(
      pressupostos = list(
        P("Com **Correção** `nenhuma`, os pares são tratados como **independentes**; com autocorrelação o teste rejeita demais (medido, AR(1) phi = 0,6: 23% a 25% a 5%). Com `bootstrap_blocos`, a dependência é de **curto alcance** (cabe em blocos de √n): medido, 3,8% a 6,6% (phi 0,3 e 0,6; n = 60 e 120).",
          verificar = c("series/acf", "series/ljung_box"),
          se_falhar = "Troque a **Correção** para `bootstrap_blocos`, ou use o `series/mann_kendall` com a mesma correção (mais poder)."),
        monotona, continua,
        P("Sobram **pelo menos 6 pares** depois dos empates (o bloco pede 16 observações); com menos, nem o resultado mais extremo rejeita a 5%.")),
      referencias = list(
        R(autores = c("Cox, D. R.", "Stuart, A."), ano = 1955,
          titulo = "Some quick sign tests for trend in location and dispersion",
          fonte = "Biometrika, 42(1-2), 80-95", doi = "10.1093/biomet/42.1-2.80"),
        L$morettin,
        R(autores = c("Kundzewicz, Z. W.", "Robson, A. J."), ano = 2004,
          titulo = "Change detection in hydrological records—a review of the methodology",
          fonte = "Hydrological Sciences Journal, 49(1), 7-19", doi = "10.1623/hysj.49.1.7.53993"),
        I("trama.series", "tr_series_cox_stuart", "Cálculo próprio: pares em terços (original) ou metades, empates descartados; `stats::binom.test` com menos de 20 pares, aproximação normal a partir de 20. `bootstrap_blocos`: soma dos sinais dos pares (mesmo pareamento) em blocos móveis de round(√n), 1999 reamostras, semente do nó; conferido contra o mesmo bootstrap escrito à mão com a mesma semente (igual exatamente)."))),

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
        P("Com **Correção** `nenhuma`, sob H0 as observações são **independentes**: autocorrelação positiva imita ponto de mudança (medido, AR(1) phi = 0,6: 46% a 55% a 5%). Com `bootstrap_blocos`, a dependência é de **curto alcance** (cabe em blocos de √n): medido, 3,5% a 4,6% com phi = 0,3 e 7,8% a 8,7% com phi = 0,6 (n = 60 e 120).",
          verificar = c("series/acf", "series/ljung_box"),
          se_falhar = "Troque a **Correção** para `bootstrap_blocos`; com autocorrelação forte, leia um \"rejeita\" apertado com cautela."),
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
        R(autores = c("Kundzewicz, Z. W.", "Robson, A. J."), ano = 2004,
          titulo = "Change detection in hydrological records—a review of the methodology",
          fonte = "Hydrological Sciences Journal, 49(1), 7-19", doi = "10.1623/hysj.49.1.7.53993"),
        I("trama.series", "tr_series_pettitt", "Cálculo próprio: U_t acumulado de Mann-Whitney, K = max|U_t|, p ≈ 2·exp(−6K²/(n³ + n²)) limitado a 1. `bootstrap_blocos`: K em blocos móveis de round(√n), 1999 reamostras, p = (1 + #{K* ≥ K})/(B + 1), semente do nó; conferido contra o mesmo bootstrap escrito à mão com a mesma semente (igual exatamente)."))),

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
        P("Sob H0 a série é **ruído branco gaussiano** (independente, normal, variância constante) em torno de uma reta (`remover = reta`) ou de uma média (`media`, a formulação de Fisher 1929): é daí que sai a distribuição exata de g sobre as m = (N − 1) ÷ 2 ordenadas de Fourier, sem a de Nyquist.",
          verificar = c("series/acf", "series/ljung_box"),
          se_falhar = "Autocorrelação sem ciclo (um AR, por exemplo) já concentra potência nas frequências baixas e faz o teste rejeitar: leia o período do pico."),
        P("A tendência é **no máximo linear** (com `reta`, o periodograma a remove; com `media`, nem isso). Tendência curva ou estrutura de baixa frequência vira um pico no período da série inteira.",
          verificar = "series/plot",
          se_falhar = "Diferencie antes (`series/diff`) e desconfie de pico que não se repete (a `nota` avisa)."),
        P("A periodicidade cai **na grade de Fourier** (período N/j) e há **uma** dominante: o teste olha só o maior pico.",
          verificar = c("series/acf", "series/seasonal_plot"))),
      referencias = list(
        R(autores = "Fisher, R. A.", ano = 1929, titulo = "Tests of significance in harmonic analysis",
          fonte = "Proceedings of the Royal Society of London. Series A, 125(796), 54-59",
          doi = "10.1098/rspa.1929.0151"),
        L$morettin,
        R(autores = c("Wichert, S.", "Fokianos, K.", "Strimmer, K."), ano = 2004,
          titulo = "Identifying periodically expressed transcripts in microarray time series data",
          fonte = "Bioinformatics, 20(1), 5-20", doi = "10.1093/bioinformatics/btg364"),
        I("trama.series", "tr_series_fisher", "Cálculo próprio sobre `stats::spec.pgram(taper = 0, detrend = remover == \"reta\", fast = FALSE)`, sem a ordenada de Nyquist: g = maior ordenada / soma, p pela série exata de Fisher (todos os termos), zα = quantil exato a 5%. Conferido contra `GeneCycle::fisher.g.test` (igual a 1e-10).")))
  )
}
