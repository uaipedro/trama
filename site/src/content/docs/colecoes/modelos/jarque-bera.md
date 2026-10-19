---
title: Jarque-Bera
description: "Jarque-Bera: a assimetria e a curtose de uma coluna são as da normal?"
section: colecoes
collection: modelos
node: models/jarque_bera
category: testes
related: [models/shapiro, data/describe]
---

## O que o bloco faz

`models/jarque_bera` testa a normalidade de uma coluna pela assimetria e pela curtose: JB = n/6 (g1² + g2²/4), com os momentos de divisor n, comparado a uma qui-quadrado com 2 graus de liberdade. A saída é `data/test`, com n, assimetria e curtose em excesso na nota.

## Quando usar

Em séries longas e em amostras grandes. A qui-quadrado é assintótica: com poucas dezenas de valores, use o `models/shapiro`.

## Configuração

Informe Variável (coluna numérica).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("jb", "models/jarque_bera", variavel = "mpg", from = "carros")
```

## Como interpretar

A hipótese nula é normalidade. A nota diz se o desvio vem da assimetria ou da curtose. Não rejeitar não demonstra normalidade.
