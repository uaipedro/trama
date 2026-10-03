---
title: ETS
description: "Suavização exponencial em espaço de estados (erro, tendência, sazonalidade)."
section: colecoes
collection: series-temporais
node: series/ets
category: Modelar
order: 2
related: [series/holt_winters, series/arima, series/baseline]
---

## O que o bloco faz

Ajusta um modelo de suavização exponencial — a família que inclui Holt e
Holt-Winters — na formulação em espaço de estados de Hyndman et al.
(`forecast::ets`), com escolha por AICc e intervalos de previsão de verdade.

### O código do modelo

Três letras, na ordem da literatura:

1. **erro** — `A` aditivo, `M` multiplicativo;
2. **tendência** — `N` nenhuma, `A` aditiva, `M` multiplicativa;
3. **sazonalidade** — `N` nenhuma, `A` aditiva, `M` multiplicativa.

`Z` em qualquer posição deixa o ajuste escolher. `ZZZ` (o padrão) escolhe
tudo; `MAM` fixa erro e sazonalidade multiplicativos com tendência aditiva,
que costuma servir para `AirPassengers`. O resumo do card mostra o modelo
escolhido no mesmo formato (`ETS(M,Ad,M)`, onde `Ad` é a tendência
amortecida), e é esse código que se copia para fixar.

Sazonalidade `A` ou `M` pede série sazonal com dois ciclos, e frequência até
24 — acima disso o ETS não modela sazonalidade (use `Z` ou `N`, ou decomponha
antes). Com `Z`, numa série de frequência maior que 24, a sazonalidade é
simplesmente deixada de fora.

### Tendência amortecida

Uma tendência que perde força ao longo do horizonte, em vez de seguir reta
para sempre. Costuma prever melhor a longo prazo. `auto` deixa o ajuste
decidir; `sim` com tendência `N` é combinação proibida, e o nó para.

## Quando usar

Ajuste um modelo de suavização exponencial em espaço de estados quando nível, tendência e sazonalidade podem ser atualizados ao longo do tempo.

## Configuração

- **Modelo** — o código de três letras. Obrigatório: em branco, o nó para.
- **Tendência amortecida** — `auto`, `sim` ou `não`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("ets", "series/ets", modelo = "MAM", amortecida = "auto", from = "pax") |>
  tr_add("prev", "series/forecast", from = "ets")
```

## Como interpretar

Um modelo (`series/model`), com o nome `ETS(…)` no resumo.

## Veja também

`series/holt_winters` para a versão clássica; `series/arima` para a outra
grande família; `series/baseline` para a referência que o modelo tem de bater.
