---
title: ARIMA / SARIMA
description: "Ajusta um ARIMA ou SARIMA (com parte sazonal), automático ou com a ordem escolhida; estima as intervenções declaradas antes."
section: colecoes
collection: series-temporais
node: series/arima
category: Modelar
order: 2
related: [series/forecast, series/residuals, series/ljung_box, series/ndiffs, series/acf, series/pacf, series/intervencao, series/detect_interventions, series/ets]
---

## O que o bloco faz

Ajusta um modelo ARIMA(p,d,q)(P,D,Q)[ciclo]: a série depois de `d` diferenças
simples e `D` sazonais é explicada pelos seus `p` valores anteriores (AR), pelos
`q` erros anteriores (MA), e pelas versões sazonais disso (`P`, `Q`), uma
defasagem de ciclo por vez.

### Automático

Com **Automático** ligado, a ordem é escolhida pelo algoritmo de Hyndman e
Khandakar (`forecast::auto.arima`): testes de raiz unitária decidem `d` e `D`, e
uma busca pelo menor AICc decide o resto. **As seis ordens do card são
ignoradas** — e o card as esconde enquanto ele está ligado.
O resumo do card mostra a ordem escolhida (`ARIMA(0,1,1)(0,1,1)[12]`), e o
caminho natural é rodar automático, ler, e só então fixar à mão se quiser.

**Parte sazonal** desligada restringe a busca a modelos sem P, D, Q. Só vale no
automático.

### Manual

Com **Automático** desligado, o modelo é exatamente o das seis ordens. P, D ou
Q maiores que zero numa série sem ciclo param o nó em vermelho.

**Constante / deriva** permite a média (sem diferença) ou a deriva (com uma
diferença) — uma tendência linear na previsão. Com duas diferenças ela não é
estimada, porque não é identificável.

Transformação (log, Box-Cox) não é param daqui: é o `series/transform` antes,
visível no fio.

### SARIMA

A parte sazonal (P, D, Q) usa o ciclo da série: 12 numa mensal, 4 numa
trimestral. Um SARIMA(0,1,1)(0,1,1)[12] — o "modelo das companhias aéreas" de
Box e Jenkins — é Automático desligado, d = 1, q = 1, D = 1, Q = 1.

### Intervenções

Se a série chega de blocos `series/intervencao`, cada intervenção declarada
vira um termo do modelo (`degrau_1983_fev`, `pulso_1913`), estimado por máxima
verossimilhança junto com o ARMA (Box e Tiao, 1975): o erro-padrão do efeito já
leva em conta a autocorrelação. Pulso, degrau e rampa entram como regressores;
com diferenças, o regressor é diferenciado junto e o ω continua sendo o efeito
na série original. O **inovacional** entra filtrado pelos pesos ψ do próprio
modelo, recalculados até o ψ do regressor ser o do ajuste que ele produz
(ponto fixo, até a precisão do otimizador; o erro-padrão de ω trata ψ como
conhecido). A **gradual** tem o δ
estimado por verossimilhança perfilada e entra no AIC; o card mostra δ e o
efeito de longo prazo, e `models/coefficients` dá os erros-padrão com a
covariância completa.

No Automático com intervenções, a busca escolhe a ordem com os regressores na
forma imediata; com inovacional ou gradual, a ordem escolhida é reajustada com
eles inteiros. A previsão (`series/forecast`) estende cada efeito sozinha: o
pulso volta a zero, o degrau fica, a rampa segue. O intervalo `bootstrap` não
vale com intervenções (o `forecast` não simula com os regressores); use o
`normal`. ETS, Holt-Winters e `series/regression` não estimam intervenções e
recusam a série que chega com elas.

Com intervenções, este bloco não aceita faltantes: série com buraco põe o nó
em vermelho. Ligue um `series/interpolate` antes.

Falha de ajuste ("non-stationary AR part") para o nó com a mensagem do
`forecast` logo abaixo da nossa.

## Parâmetros

- **Automático** — escolhe a ordem sozinho, ignorando as seis ordens.
- **Parte sazonal (automático)** — permite P, D, Q na busca automática.
- **Constante / deriva** — permite média ou deriva no modelo.
- **p**, **d**, **q** — ordens da parte não sazonal (só no manual).
- **P**, **D**, **Q** — ordens da parte sazonal (só no manual).

## Valor

Um modelo (`series/model`). O card mostra os coeficientes (e as intervenções,
se houver); o resumo, o nome do modelo, o AIC e o desvio dos resíduos.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("arima", "series/arima", from = "log") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "arima")
```

## Veja também

`series/forecast` para prever; `series/residuals` e `series/ljung_box` para
diagnosticar; `series/ndiffs`, `series/acf` e `series/pacf` para escolher a
ordem à mão; `series/intervencao` para declarar eventos;
`series/detect_interventions` para procurá-los; `series/ets` para a
alternativa por suavização exponencial.

