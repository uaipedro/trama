---
title: Shapiro-Wilk
description: "Shapiro-Wilk: uma coluna tem distribuição normal?"
section: colecoes
collection: modelos
node: models/shapiro
category: testes
related: [models/chisq, models/cor_test]
---

## O que o bloco faz

`models/shapiro` Aplica Shapiro–Wilk a uma coluna (3–5000 observações). A saída é `models/test`.

## Quando usar

Use para avaliar a normalidade de uma coluna observada; para pressuposto de regressão, escolha o teste de resíduos.

## Configuração

Informe Coluna numérica.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("resultado", "models/shapiro", coluna = "weight", from = "dados")
```

Com `PlantGrowth`, o resultado resume Shapiro–Wilk para os 30 valores de `weight`; a estatística W mede o afastamento da forma normal.

## Como interpretar

A hipótese nula é normalidade. Não rejeição não demonstra normalidade, especialmente com amostras pequenas.
