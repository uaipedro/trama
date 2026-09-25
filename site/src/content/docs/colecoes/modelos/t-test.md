---
title: t para duas amostras
description: "t de Welch (ou de Student): as médias de dois grupos independentes são iguais?"
section: colecoes
collection: modelos
node: models/t_test
category: testes
related: [models/paired_t, models/wilcoxon, models/cohen_d]
---

## O que o bloco faz

`models/t_test` Compara médias de dois grupos independentes pelo teste t. A saída é `data/test`.

## Quando usar

Use para comparar a média de uma resposta numérica em dois grupos independentes.

## Configuração

Informe resposta numérica e grupo com dois níveis. Variâncias iguais seleciona Student; desligado, Welch. Alternativa define direção.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "ToothGrowth") |>
  tr_add("resultado", "models/t_test", resposta = "len", grupo = "supp", from = "dados")
```

`ToothGrowth` tem dois suplementos e medidas independentes de comprimento dentário.

## Como interpretar

Welch não exige variâncias iguais; Student combina as variâncias. A hipótese nula compara médias.
