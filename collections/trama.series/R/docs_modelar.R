# Pressupostos e referências: modelos, previsão, referência e acurácia.

.tr_series_docs_modelar <- function() {
  P <- .tr_series_P; I <- .tr_series_impl; R <- trama::tr_ref
  L <- .tr_series_livros()
  residuo_branco <- function(o_que = "Os resíduos do modelo") P(
    sprintf("%s são **ruído branco**: sem autocorrelação sobrando. Autocorrelação nos resíduos quer dizer estrutura que o modelo deixou para trás, e intervalos estreitos demais.", o_que),
    verificar = c("series/residuals", "series/ljung_box", "series/acf"),
    se_falhar = "Ligue `series/residuals` → `series/ljung_box` (com `graus` = parâmetros estimados). Se rejeitar, mude a ordem do `series/arima` (veja a `series/acf` e a `series/pacf` dos resíduos) ou troque de família (`series/ets`).")
  sem_quebra <- P(
    "O processo que gerou a série é **o mesmo do começo ao fim**: sem quebra estrutural nem intervenção no meio. O modelo aprende uma dinâmica só, e uma mudança de regime a contamina.",
    verificar = c("series/plot", "series/pettitt", "series/zivot_andrews"),
    se_falhar = "Ajuste só o trecho depois da quebra (`series/window`), ou, com a data conhecida, meça o efeito com `series/intervencao` (ARIMA com degrau, pulso ou rampa).")
  hyndman_khandakar <- R(autores = c("Hyndman, R. J.", "Khandakar, Y."), ano = 2008,
                         titulo = "Automatic time series forecasting: the forecast package for R",
                         fonte = "Journal of Statistical Software, 27(3), 1-22",
                         doi = "10.18637/jss.v027.i03")
  hyndman_ets <- R(autores = c("Hyndman, R. J.", "Koehler, A. B.", "Snyder, R. D.", "Grose, S."),
                   ano = 2002,
                   titulo = "A state space framework for automatic forecasting using exponential smoothing methods",
                   fonte = "International Journal of Forecasting, 18(3), 439-454",
                   doi = "10.1016/S0169-2070(01)00110-8")
  holt <- R(autores = "Holt, C. C.", ano = 2004,
            titulo = "Forecasting seasonals and trends by exponentially weighted moving averages",
            fonte = "International Journal of Forecasting, 20(1), 5-10 (reedição do memorando de 1957)",
            doi = "10.1016/j.ijforecast.2003.09.015")
  winters <- R(autores = "Winters, P. R.", ano = 1960,
               titulo = "Forecasting sales by exponentially weighted moving averages",
               fonte = "Management Science, 6(3), 324-342", doi = "10.1287/mnsc.6.3.324")
  list(
    "series/arima" = list(
      pressupostos = list(
        P("Depois de `d` diferenças simples e `D` sazonais, a série é **estacionária**: média e autocovariância que não mudam com o tempo. É o que a parte ARMA modela; com diferenças de menos o ajuste falha (\"non-stationary AR part\") ou prevê mal.",
          verificar = c("series/ndiffs", "series/adf", "series/kpss"),
          se_falhar = "Use o `series/ndiffs` para o número de diferenças e fixe `d` e `D` (ou deixe o automático, que decide `d` pelo KPSS e `D` pela força sazonal). Diferenças DEMAIS também é erro: aparece como MA com coeficiente perto de -1."),
        P("A **variância é constante** no tempo. Oscilação que cresce com o nível não é modelada pelo ARIMA.",
          verificar = "series/plot",
          se_falhar = "Transforme antes com `series/transform` (log ou Box-Cox) e modele a série transformada."),
        P("O **ciclo declarado** da série (a frequência) é o período sazonal verdadeiro: a parte sazonal (P, D, Q) usa defasagens múltiplas dele.",
          verificar = c("series/seasonal_plot", "series/acf", "series/periodicity_fisher"),
          se_falhar = "Declare a frequência certa no nó que cria a série (`series/from_table`)."),
        residuo_branco(),
        P("Para os **intervalos de previsão**, os resíduos são **normais**; a previsão pontual não depende disso.",
          verificar = c("series/residuals", "view/qq", "models/shapiro"),
          se_falhar = "A série de resíduos vira tabela no fio (coluna `valor`). Se não forem normais, use **Intervalo** = `bootstrap` no `series/forecast`."),
        sem_quebra),
      referencias = list(L$box_jenkins, hyndman_khandakar, L$morettin, L$fpp3,
        I("forecast", "auto.arima",
          "Com Automático: busca passo a passo pelo menor AICc; `d` pelo KPSS e `D` pela força sazonal (padrões do pacote); `seasonal`, `allowdrift` e `allowmean` vêm dos params. Manual: `forecast::Arima(order, seasonal, include.constant)`, por máxima verossimilhança."))),

    "series/intervencao" = list(
      pressupostos = list(
        P("A **data** da intervenção é conhecida de antemão, e não escolhida pelo maior salto da própria série: escolhê-la pelo dado e testá-la no mesmo dado torna o p-valor otimista.",
          verificar = "series/plot",
          se_falhar = "Para procurar a data, `series/pettitt` ou `series/zivot_andrews`; depois confirme em outra série ou período."),
        P("Fora da intervenção, a série é um **ARIMA estável** da ordem dada (estacionário depois das diferenças) e a dinâmica é a mesma antes e depois — só o nível (degrau), um período (pulso) ou a inclinação (rampa) muda.",
          verificar = c("series/window", "series/arima", "series/ndiffs"),
          se_falhar = "Identifique a ordem no trecho anterior (`series/window` → `series/arima` automático) e use-a aqui."),
        P("Com **Resposta** = `imediata`, o efeito entra **inteiro na data** (forma de ordem zero); com `gradual`, segue ω/(1 − δB) com |δ| < 1 — uma resposta de outra forma (duas taxas, atraso) não é modelada.",
          verificar = c("series/plot", "series/residuals"),
          se_falhar = "Se o resíduo mostra o efeito chegando aos poucos, use `gradual`; se δ vai para a borda, troque o degrau pela rampa."),
        residuo_branco(),
        P("O IC e o p-valor são de **Wald** (normal assintótica da máxima verossimilhança): pedem resíduos aproximadamente normais e série não muito curta.",
          verificar = c("series/residuals", "view/qq"))),
      referencias = list(
        R(autores = c("Box, G. E. P.", "Tiao, G. C."), ano = 1975,
          titulo = "Intervention analysis with applications to economic and environmental problems",
          fonte = "Journal of the American Statistical Association, 70(349), 70-79",
          doi = "10.1080/01621459.1975.10480264"),
        L$box_jenkins, L$fpp3,
        R(autores = c("Cryer, J. D.", "Chan, K.-S."), ano = 2008,
          titulo = "Time Series Analysis: With Applications in R", fonte = "2. ed. New York: Springer (cap. 11)",
          doi = "10.1007/978-0-387-75959-3"),
        I("forecast", "Arima",
          "`xreg` = o regressor (degrau, pulso ou rampa), `order`, `seasonal`, `include.constant` dos params; máxima verossimilhança. Erro-padrão de `var.coef`, IC e p de Wald; conferido contra a mesma chamada a 1e-8. Resposta gradual: regressor filtrado x_t = I_t + δx_{t−1}, δ pela verossimilhança perfilada (`optimize` em (−0,999; 0,999)), erro-padrão pela hessiana numérica (`optimHess`) da verossimilhança completa; conferido contra `TSA::arimax(transfer = list(c(1, 0)))` 1.3.1 no airmiles (degrau e pulso em 2001-09) a 1e-3."))),

    "series/ets" = list(
      pressupostos = list(
        P("A série é descrita por **nível, tendência e sazonalidade** que evoluem por suavização exponencial, com o erro entrando de forma aditiva (A) ou multiplicativa (M). Erro, tendência ou sazonalidade **multiplicativos pedem série positiva**.",
          verificar = "series/plot",
          se_falhar = "Com zeros ou negativos, fixe as letras em `A` (ou use `Z`, que as evita) ou modele com `series/arima`."),
        P("O **ciclo sazonal** é o declarado na série, e de **no máximo 24** períodos.",
          verificar = c("series/seasonal_plot", "series/periodicity_fisher"),
          se_falhar = "Ciclo maior que 24: sazonalidade `N`, ou `series/stl` e modelar a dessazonalizada (`series/component`)."),
        residuo_branco(),
        P("Para os **intervalos de previsão**, o erro é **normal** e independente (é a verossimilhança do modelo em espaço de estados).",
          verificar = c("series/residuals", "view/qq", "models/shapiro"),
          se_falhar = "A série de resíduos vira tabela no fio (coluna `valor`). Sem normalidade, leia os intervalos como aproximados."),
        sem_quebra),
      referencias = list(hyndman_ets, L$fpp3,
        I("forecast", "ets",
          "`model` é o código de três letras do param (Z = escolha por AICc) e `damped` sai de `amortecida` (auto = NULL, o ajuste escolhe). Parâmetros por máxima verossimilhança."))),

    "series/holt_winters" = list(
      pressupostos = list(
        P("A série tem **nível, tendência localmente linear e sazonalidade de ciclo fixo** (as chaves ligadas), que mudam devagar: é o que três equações de suavização conseguem acompanhar.",
          verificar = c("series/plot", "series/seasonal_plot")),
        P("Na sazonalidade **multiplicativa**, a amplitude da oscilação é **proporcional ao nível**; na aditiva, constante.",
          verificar = "series/plot",
          se_falhar = "Troque o tipo, ou use `series/transform` (log) e a aditiva."),
        residuo_branco("Os erros de previsão de um passo"),
        sem_quebra),
      referencias = list(holt, winters, L$morettin, L$fpp3,
        I("stats", "HoltWinters",
          "α, β e γ minimizam a soma dos quadrados dos erros de um passo (`optim`); `beta = FALSE` e `gamma = FALSE` desligam tendência e sazonalidade. Sem verossimilhança: não há AIC."))),

    "series/forecast" = list(
      pressupostos = list(
        P("O modelo ajustado é **adequado**: resíduos sem autocorrelação. O intervalo supõe que o que sobrou é ruído.",
          verificar = c("series/residuals", "series/ljung_box"),
          se_falhar = "Refaça o modelo (ordem do ARIMA, família) antes de ler o leque."),
        P("Os resíduos são **normais e de variância constante**: os limites de 80% e 95% são quantis normais.",
          verificar = c("series/residuals", "view/qq", "models/shapiro"),
          se_falhar = "A série de resíduos vira tabela no fio (coluna `valor`). Variância crescente: modele o log (`series/transform`). Sem normalidade, use **Intervalo** = `bootstrap` (ARIMA e ETS), que reamostra os resíduos e ainda supõe independência e variância constante."),
        P("O **futuro segue a mesma dinâmica** do passado usado no ajuste: nenhuma quebra, intervenção ou mudança de regime no horizonte. Quanto maior o horizonte, mais essa suposição pesa.",
          se_falhar = "Não há teste possível para o futuro; encurte o horizonte e compare com o `series/baseline` num período de teste (`series/window` + `series/accuracy`)."),
        P("O intervalo trata os **parâmetros estimados como conhecidos**: no ARIMA e no ETS a incerteza da estimação não entra, e o leque sai um pouco estreito em série curta.")),
      referencias = list(L$fpp3, L$box_jenkins,
        I("forecast", "forecast",
          "`h` = horizonte, `level = c(80, 95)` fixos; com `bootstrap`, `bootstrap = TRUE, npaths = 5000` sob a semente do nó (Mersenne-Twister), conferido contra a mesma chamada a 1e-12. Série transformada antes é prevista na escala transformada."))),

    "series/baseline" = list(
      pressupostos = list(
        P("Cada método supõe uma série de um tipo: **média** — série sem tendência nem sazonalidade, em torno de um nível fixo; **ingênuo** e **deriva** — passeio aleatório (com deriva constante, no segundo); **ingênuo sazonal** — o padrão de um ciclo se repete no seguinte.",
          verificar = c("series/plot", "series/seasonal_plot")),
        P("Os **intervalos** supõem erros **normais e sem autocorrelação** do método de referência (são fórmulas fechadas, não simulação).",
          verificar = c("series/ljung_box", "view/qq"),
          se_falhar = "Para comparar métodos, o que conta é o erro no teste (`series/accuracy`), não o leque.")),
      referencias = list(L$fpp3,
        I("forecast", "snaive",
          "Conforme o método: `forecast::meanf`, `forecast::naive`, `forecast::snaive` ou `forecast::rwf(drift = TRUE)`, todos com `level = c(80, 95)`."))),

    "series/accuracy" = list(
      pressupostos = list(
        P("A validação respeita a **ordem do tempo**: o modelo foi ajustado num trecho ANTERIOR (`series/window`) e a série real cobre os períodos que ele não viu. Sem isso, o erro de teste mede o que o modelo decorou (vazamento).",
          verificar = "series/window",
          se_falhar = "Recorte o treino com `series/window` (fim antes do teste) e ligue a série inteira em **real**. Nunca embaralhe observações de série temporal."),
        P("A linha de **treino** é erro de um passo dentro da amostra: otimista por construção, não serve para escolher modelo."),
        P("O **MAPE** e o **MPE** pedem série **positiva e longe de zero** (dividem pelo valor observado); com zeros eles explodem ou saem infinitos.",
          se_falhar = "Leia o MASE ou o MAE."),
        P("O **MASE** escala pelo erro do ingênuo (sazonal, na série com ciclo) no treino: supõe treino com mais de um ciclo e sem ser constante.")),
      referencias = list(
        R(autores = c("Hyndman, R. J.", "Koehler, A. B."), ano = 2006,
          titulo = "Another look at measures of forecast accuracy",
          fonte = "International Journal of Forecasting, 22(4), 679-688",
          doi = "10.1016/j.ijforecast.2006.03.001"),
        L$fpp3,
        I("forecast", "accuracy",
          "Sem **real**: `accuracy(previsao)`, só o treino. Com **real**: `accuracy(previsao, real)`, que compara nos períodos em comum; a coleção recusa antes se não houver nenhum.")))
  )
}
