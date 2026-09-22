---
title: ANOVA · fatorial
description: "Análise de variância de um fatorial com 2 ou 3 fatores, em DIC ou em blocos."
section: colecoes
collection: modelos
node: models/anova_factorial
category: anova
related: [models/emmeans, models/anova_split_plot]
---

## O que o bloco faz

`models/anova_factorial` Ajusta efeitos principais e todas as interações de dois ou três fatores. A saída é `models/fit`.

## Quando usar

Use para avaliar dois ou três fatores no mesmo experimento e verificar se seus efeitos dependem uns dos outros.

## Configuração

Informe resposta, duas ou três colunas em Fatores e, opcionalmente, Bloco.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "ToothGrowth") |>
  tr_add("ajuste", "models/anova_factorial", resposta = "len", fatores = "supp, dose", from = "dados")
```

`ToothGrowth` fornece comprimento dentário, suplemento e dose para o fatorial 2 × 3.

## Como interpretar

Interação indica que o efeito de um fator depende do nível de outro. Com interação relevante, interprete médias desdobradas por Por.
