---
title: Gráfico de lagarta
description: "Os efeitos aleatórios previstos de um misto, um por nível do grupo, com intervalo e a faixa do desvio padrão."
section: colecoes
collection: modelos
node: models/plot_caterpillar
category: resumir
related: [models/anova_table, models/coefficients]
---

## O que o bloco faz

`models/plot_caterpillar` Ordena efeitos aleatórios por grupo e mostra intervalos. A saída é `view/plot`.

## Quando usar

Use para comparar efeitos previstos entre níveis de um grupo aleatório e localizar níveis distantes da média.

## Configuração

Grupo seleciona o termo aleatório; intervalo, faixa de desvio padrão e ordenação controlam a exibição.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "sleepstudy") |>
  tr_add("ajuste", "models/lmer", formula = "Reaction ~ Days + (Days | Subject)", from = "dados") |>
  tr_add("resultado", "models/plot_caterpillar", grupo = "Subject", from = "ajuste")
```

No ajuste `Reaction ~ Days + (Days | Subject)`, o gráfico ordena os efeitos de sujeito e mostra seus intervalos; valores que cruzam zero não se distinguem claramente do intercepto médio.

## Como interpretar

Zero é o efeito médio populacional. Intervalo contendo zero não distingue aquele nível do valor médio no nível de confiança mostrado.
