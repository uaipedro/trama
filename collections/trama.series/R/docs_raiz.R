# Pressupostos e referências: raiz unitária, estacionariedade e ruído branco.

.tr_series_docs_raiz <- function() {
  P <- .tr_series_P; I <- .tr_series_impl; R <- trama::tr_ref
  L <- .tr_series_livros()
  deterministico <- P(
    "Os **termos determinísticos** escolhidos (constante, ou constante e tendência) são os da série: sem a tendência quando ela existe, o teste confunde tendência com raiz unitária; com tendência sobrando, perde poder.",
    verificar = "series/plot",
    se_falhar = "Série que sobe ou desce de forma regular: `tendência`. Série que oscila em torno de um nível: `constante`.")
  sem_quebra <- P(
    "A série **não tem quebra estrutural** (mudança de nível ou de inclinação). Com quebra, os testes de raiz unitária tendem a NÃO rejeitar, e o KPSS a rejeitar, mesmo com a série estacionária dos dois lados.",
    verificar = c("series/plot", "series/pettitt"),
    se_falhar = "`series/zivot_andrews`, que admite uma quebra e a estima.")
  poder <- P(
    "Série **longa o bastante**: com poucas observações o teste tem pouco poder, e \"não rejeita\" não é evidência a favor de H0.",
    se_falhar = "Leia junto com o teste de hipótese nula oposta (`series/adf` × `series/kpss`); se discordarem, diferenciar é o lado seguro.")
  dickey_fuller <- R(autores = c("Dickey, D. A.", "Fuller, W. A."), ano = 1979,
                     titulo = "Distribution of the estimators for autoregressive time series with a unit root",
                     fonte = "Journal of the American Statistical Association, 74(366a), 427-431",
                     doi = "10.1080/01621459.1979.10482531")
  box_teste <- function(tipo, ref, nota_extra) list(
    pressupostos = list(
      P("A série testada é **estacionária** (média e variância constantes): numa série com tendência ou raiz unitária as autocorrelações amostrais ficam altas por construção, e o teste rejeita sem dizer nada sobre ruído.",
        verificar = c("series/plot", "series/kpss"),
        se_falhar = "Teste os resíduos de um modelo (`series/residuals`) ou a série diferenciada (`series/diff`)."),
      P("Nos **resíduos de um modelo**, `graus` é o número de parâmetros ARMA estimados (p + q + P + Q): sem o desconto o qui-quadrado tem graus demais e o teste aceita ruído branco onde há estrutura. A contagem é manual, lida nos coeficientes do card do modelo.",
        verificar = "series/residuals"),
      P("As **defasagens** cobrem a dependência que importa (dois ciclos, na série sazonal); dependência além da última defasagem o teste não olha.",
        verificar = "series/acf"),
      P(sprintf("A distribuição qui-quadrado de Q é **assintótica**. %s", nota_extra))),
    referencias = list(ref, L$box_jenkins, L$morettin,
      I("stats", "Box.test", sprintf("`type = \"%s\"`, `lag` = defasagens (0 = regra de Hyndman: 10, ou dois ciclos, até n/5), `fitdf` = graus. Faltantes ignorados na autocorrelação.", tipo))))
  list(
    "series/adf" = list(
      pressupostos = list(
        P("A série segue um **autorregressivo** de ordem finita, e as defasagens da diferença bastam para deixar o erro da regressão **sem autocorrelação** (é o \"aumentado\").",
          verificar = "series/pacf",
          se_falhar = "Aumente o teto de **Defasagens**, ou use o `series/phillips_perron`, que corrige a estatística em vez de acrescentar defasagens."),
        P("Há **no máximo uma** raiz unitária: com duas (série que precisa de duas diferenças), o teste na série em nível não se lê.",
          verificar = "series/ndiffs",
          se_falhar = "Aplique o teste à série já diferenciada (`series/diff`)."),
        deterministico, sem_quebra, poder),
      referencias = list(dickey_fuller,
        R(autores = c("Said, S. E.", "Dickey, D. A."), ano = 1984,
          titulo = "Testing for unit roots in autoregressive-moving average models of unknown order",
          fonte = "Biometrika, 71(3), 599-607", doi = "10.1093/biomet/71.3.599"),
        L$morettin,
        I("urca", "ur.df", "`type = \"drift\"` (constante) ou `\"trend\"`, `lags` = teto (0 = trunc((n - 1)^(1/3))), `selectlags = \"AIC\"`. Decide pela estatística tau e a tabela de críticos do pacote; sem p-valor."))),

    "series/kpss" = list(
      pressupostos = list(
        P("Sob H0 a série é **estacionária em torno de um nível** (ou de uma reta, com `tendência`) mais um erro de **dependência curta**, que a variância de longo prazo corrige com a janela de defasagens.",
          verificar = "series/acf",
          se_falhar = "Autocorrelação forte e persistente em série estacionária faz o KPSS rejeitar demais; confira com o `series/adf`."),
        deterministico, sem_quebra, poder),
      referencias = list(
        R(autores = c("Kwiatkowski, D.", "Phillips, P. C. B.", "Schmidt, P.", "Shin, Y."), ano = 1992,
          titulo = "Testing the null hypothesis of stationarity against the alternative of a unit root: how sure are we that economic time series have a unit root?",
          fonte = "Journal of Econometrics, 54(1-3), 159-178",
          doi = "10.1016/0304-4076(92)90104-Y"),
        L$morettin,
        I("urca", "ur.kpss", "`type = \"mu\"` (constante) ou `\"tau\"` (tendência), `lags = \"short\"` (janela trunc(4·(n/100)^(1/4))). Decide pela tabela de críticos; sem p-valor."))),

    "series/phillips_perron" = list(
      pressupostos = list(
        deterministico,
        P("A autocorrelação do erro é de **dependência curta**, corrigida de forma não paramétrica (Newey-West com janela curta). Com componente MA forte e negativo o teste rejeita demais.",
          verificar = "series/acf",
          se_falhar = "Confira com o `series/adf` e o `series/kpss`."),
        sem_quebra, poder),
      referencias = list(
        R(autores = c("Phillips, P. C. B.", "Perron, P."), ano = 1988,
          titulo = "Testing for a unit root in time series regression",
          fonte = "Biometrika, 75(2), 335-346", doi = "10.1093/biomet/75.2.335"),
        R(autores = "Fuller, W. A.", ano = 1976, titulo = "Introduction to Statistical Time Series",
          fonte = "New York: Wiley", papel = "livro-texto"),
        R(autores = "Hamilton, J. D.", ano = 1994, titulo = "Time Series Analysis",
          fonte = "Princeton: Princeton University Press", papel = "livro-texto"),
        L$morettin,
        I("stats", "PP.test", "`tendência`: estatística Z(t) com constante e tendência, `lshort = TRUE`; p-valor interpolado na tabela τ_τ e preso em [0,01; 0,99]."),
        I("trama.series", "tr_series_phillips_perron", "`constante`: Z(t) pela forma geral (Hamilton 1994, eq. 17.6.8), Newey-West com janela trunc(4·(n/100)^(1/4)) e pesos de Bartlett; p-valor interpolado na tabela τ_μ de Fuller (1976, tab. 8.5.2) e preso em [0,01; 0,99]. Conferido contra `aTSA::pp.test` (tipo 2) a 1e-10 no Z(t)."))),

    "series/zivot_andrews" = list(
      pressupostos = list(
        P("Há **no máximo uma** quebra, do tipo escolhido em **O que quebra** (nível, inclinação ou ambas). Duas quebras, ou o tipo errado, e o teste não a enxerga.",
          verificar = c("series/plot", "series/pettitt"),
          se_falhar = "Na dúvida, `ambas`. Testes com duas quebras ainda sem bloco no trama."),
        P("A quebra está **longe das pontas** (entre 15% e 85% da série), e sob H0 a série tem raiz unitária **sem** quebra.",
          verificar = "series/plot"),
        P("As **defasagens** bastam para deixar o erro da regressão sem autocorrelação. Aqui o número é fixo (não há escolha por AIC).",
          verificar = "series/pacf",
          se_falhar = "Aumente **Defasagens** (o bloco diz o máximo que cabe)."),
        P("Série de **pelo menos 20** observações, e de preferência 40 ou mais: abaixo disso o teste rejeita mais que o nível nominal.")),
      referencias = list(
        R(autores = c("Zivot, E.", "Andrews, D. W. K."), ano = 1992,
          titulo = "Further evidence on the great crash, the oil-price shock, and the unit-root hypothesis",
          fonte = "Journal of Business & Economic Statistics, 10(3), 251-270",
          doi = "10.1080/07350015.1992.10509904"),
        L$morettin,
        I("urca", "ur.za", "`model = \"intercept\"`, `\"trend\"` ou `\"both\"`, `lag` fixo (0 = trunc((n - 1)^(1/3))). A coleção refaz o mínimo do t só nos cortes entre 15% e 85% da série e usa os críticos de `z@cval`."))),

    "series/ljung_box" = box_teste("Ljung-Box",
      R(autores = c("Ljung, G. M.", "Box, G. E. P."), ano = 1978,
        titulo = "On a measure of lack of fit in time series models",
        fonte = "Biometrika, 65(2), 297-303", doi = "10.1093/biomet/65.2.297"),
      "A correção do Ljung-Box aproxima bem já em amostras moderadas; o bloco pede pelo menos 12 observações válidas."),

    "series/box_pierce" = box_teste("Box-Pierce",
      R(autores = c("Box, G. E. P.", "Pierce, D. A."), ano = 1970,
        titulo = "Distribution of residual autocorrelations in autoregressive-integrated moving average time series models",
        fonte = "Journal of the American Statistical Association, 65(332), 1509-1526",
        doi = "10.1080/01621459.1970.10481180"),
      "Sem a correção de amostra pequena, o Box-Pierce rejeita MENOS que o nominal em série curta; para decidir, use o `series/ljung_box`.")
  )
}
