---
title: Levene
description: "Levene (Brown-Forsythe): as variâncias dos resíduos são iguais entre os tratamentos?"
section: colecoes
collection: modelos
node: models/levene
category: pressupostos
related: [models/bartlett, models/breusch_pagan]
---

## O que o bloco faz

`models/levene` Compara variâncias residuais entre grupos de efeitos fixos. A saída é `data/test`.

## Quando usar

Use para avaliar igualdade de variâncias entre grupos de tratamento a partir dos resíduos do ajuste.

## Configuração

Centro mediano é Brown–Forsythe (padrão); centro média é Levene original.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados") |>
  tr_add("resultado", "models/levene", centro = "mediana", from = "ajuste")
```

No ajuste DIC de `PlantGrowth`, Levene compara a dispersão dos resíduos entre `ctrl`, `trt1` e `trt2`, usando a mediana como centro do teste.

## Como interpretar

A hipótese nula é igualdade de variâncias. Sem fatores, use Breusch–Pagan para preditores contínuos.
