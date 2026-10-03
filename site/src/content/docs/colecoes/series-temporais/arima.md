---
title: ARIMA
description: "Ajusta um ARIMA sazonal, automático ou com a ordem escolhida."
section: colecoes
collection: series-temporais
node: series/arima
category: Modelar
order: 2
related: [series/transform, series/forecast, series/residuals]
---

## O que o bloco faz

Ajusta um modelo ARIMA(p,d,q)(P,D,Q)[ciclo]: a série depois de `d` diferenças
simples e `D` sazonais é explicada pelos seus `p` valores anteriores (AR), pelos
`q` erros anteriores (MA), e pelas versões sazonais disso (`P`, `Q`), uma
defasagem de ciclo por vez.

### Automático

Com **Automático** ligado, a ordem é escolhida pelo algoritmo de Hyndman e
Khandakar (`forecast::auto.arima`): testes de raiz unitária decidem `d` e `D`, e
uma busca pelo menor AICc decide o resto. **p**, **d**, **q**, **P**, **D** e **Q** não participam do ajuste nesse modo; os campos podem permanecer preenchidos, mas só a ordem escolhida pelo algoritmo define o modelo.
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

Falha de ajuste ("non-stationary AR part") para o nó com a mensagem do
`forecast` logo abaixo da nossa.

## Quando usar

Ajuste um modelo ARIMA para representar dependências entre valores e erros passados e gerar previsões. Use o modo automático para selecionar ordens ou o manual quando as ordens fizerem parte da hipótese de análise.

## Configuração

- **Automático** — escolhe a ordem sozinho, ignorando as seis ordens.
- **Parte sazonal (automático)** — permite P, D, Q na busca automática.
- **Constante / deriva** — permite média ou deriva no modelo.
- **p**, **d**, **q** — ordens da parte não sazonal (só no manual).
- **P**, **D**, **Q** — ordens da parte sazonal (só no manual).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("arima", "series/arima", from = "log") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "arima")
```

## Como interpretar

Um modelo (`series/model`). O card mostra os coeficientes; o resumo, o nome do
modelo, o AIC e o desvio dos resíduos.

## Veja também

`series/forecast` para prever; `series/residuals` e `series/ljung_box` para
diagnosticar; `series/ndiffs`, `series/acf` e `series/pacf` para escolher a
ordem à mão; `series/ets` para a alternativa por suavização exponencial.
