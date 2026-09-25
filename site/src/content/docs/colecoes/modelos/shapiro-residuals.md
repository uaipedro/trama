---
title: Normalidade dos resíduos
description: "Shapiro-Wilk nos resíduos do modelo: os erros são normais?"
section: colecoes
collection: modelos
node: models/shapiro_residuals
category: pressupostos
related: [models/levene, models/plot_diagnostics]
---

## O que o bloco faz

`models/shapiro_residuals` Aplica Shapiro–Wilk aos resíduos do ajuste (resíduo b em parcela subdividida). A saída é `data/test`.

## Quando usar

Use para avaliar normalidade dos erros de um ajuste linear; não use resposta bruta como substituto dos resíduos.

## Configuração

Não há parâmetros. O teste se aplica a 3–5000 resíduos e não se aplica a GLM.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados") |>
  tr_add("resultado", "models/shapiro_residuals", from = "ajuste")
```

O ajuste DIC de `PlantGrowth` fornece os resíduos de `weight ~ group`; Shapiro–Wilk retorna W e p-valor para esses 30 resíduos.

## Como interpretar

A hipótese nula é normalidade dos resíduos. Não rejeitar não comprova normalidade; leia também o Q-Q.
