---
title: Defasar
description: "Desloca a série k períodos no tempo, sem mudar os valores."
section: colecoes
collection: series-temporais
node: series/lag
category: Operar
order: 2
related: [series/lag_plot, series/diff]
---

## O que o bloco faz

Desloca a série k períodos para frente no tempo: o valor que era de janeiro
passa a ser de fevereiro (com k = 1). Os VALORES não mudam, só as datas — é
isso que uma defasagem é.

Serve para pôr uma série ao lado da própria versão atrasada: ligue a original e
a defasada em `data/join` por `tempo`, e cada linha terá `x[t]` e `x[t-k]` —
a base de um modelo com defasagem, ou de um disperso da série contra o
passado dela (para esse gráfico pronto, `series/lag_plot`).

k negativo adianta.

## Quando usar

Desloque valores no tempo para alinhar uma observação com períodos anteriores ou posteriores. Isso permite construir comparações e variáveis defasadas.

## Configuração

- **Defasagem (k)** — quantos períodos deslocar. Positivo atrasa, negativo
  adianta; zero devolve a série como está.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("atrasada", "series/lag", k = 12L, from = "pax")
```

## Como interpretar

A mesma série (`series/ts`), com início e fim deslocados k períodos.

## Veja também

`series/lag_plot` para ver a série contra ela mesma defasada; `data/join` para
alinhar original e defasada numa tabela; `series/diff`, que é a série menos a
defasada.
