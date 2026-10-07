---
title: Amplitude × média
description: "Amplitude × média: a dispersão da série cresce com o nível?"
section: colecoes
collection: series-temporais
node: series/range_mean
category: Variância
order: 1
related: [series/transform, series/plot, series/seasonal_plot]
---

## O que o bloco faz

Divide a série em blocos completos consecutivos, calcula a média e a amplitude
(máximo − mínimo) de cada bloco e testa a inclinação da regressão amplitude ~
média. É o gráfico amplitude-média clássico, com o teste t da inclinação.
O trecho final que não completa um bloco é descartado.

## Quando usar

Antes de modelar, para decidir se a série pede transformação. Inclinação
positiva com p < 0,05 sugere que a dispersão cresce com o nível: avalie log ou
Box-Cox em `series/transform` e repita o diagnóstico.

## Configuração

**tamanho** — tamanho do bloco. `0` (padrão) usa a frequência declarada da
série (12 para mensal, por exemplo); qualquer valor positivo substitui esse
padrão. São necessários ao menos três blocos completos. Uma entrada: **serie**,
que não aceita faltantes.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example", dataset = "AirPassengers") |>
  tr_add("am", "series/range_mean", from = "pax")
```

## Como interpretar

Um teste (`data/test`) com inclinação, erro-padrão, t e p, e um gráfico com os
pontos dos blocos e a reta ajustada. Um resultado não significativo não prova
variância constante: com poucos blocos o teste tem pouco poder.

Conferido contra o `rmplot` do gretl 2023c no AirPassengers (inclinação
0,560685, p = 4,78409e-10). Diverge do gretl quando a série termina no meio de
um bloco, porque o gretl usa o bloco incompleto e aqui ele sai.

## Veja também

`series/transform` para aplicar log ou Box-Cox; `series/plot` para ver a
dispersão ao longo do tempo.
