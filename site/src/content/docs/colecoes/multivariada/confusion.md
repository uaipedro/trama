---
title: Matriz de confusão
description: Cruza grupos reais e previstos e resume acertos por resubstituição ou validação cruzada.
section: colecoes
collection: multivariada
node: multi/confusion
related: [multi/discriminant, multi/logistic, multi/classify]
---

## O que o bloco faz

O bloco `multi/confusion` cruza o grupo real com o previsto e devolve contagens e taxa de acerto por resubstituição ou validação cruzada. O bloco recebe `multi/classifier`.

## Quando usar

Use **Matriz de confusão** para identificar grupos confundidos e comparar o desempenho de classificadores.

## Configuração

**Validação** — `cruzada` (padrão, deixa uma observação fora) ou `resubstituição` (avalia o próprio treino).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", grupo = "Species", from = "dados") |>
  tr_add("cm", "multi/confusion", validacao = "cruzada", from = "lda")
```

A matriz resultante conta acertos e erros por espécie usando validação cruzada.

## Como interpretar

A diagonal contém acertos e as demais células, erros por par de grupos. A validação cruzada deixa uma observação fora do ajuste por vez; resubstituição tende a ser otimista.

## Veja também

- [`Discriminante`](/trama/colecoes/multivariada/discriminant/)
- [`Regressão logística`](/trama/colecoes/multivariada/logistic/)
- [`Classificar`](/trama/colecoes/multivariada/classify/)
