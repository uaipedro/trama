---
title: ANOVA · DIC
description: "Análise de variância de um delineamento inteiramente casualizado."
section: colecoes
collection: modelos
node: models/anova_dic
category: anova
related: [models/emmeans, models/shapiro_residuals]
---

## O que o bloco faz

`models/anova_dic` Ajusta `resposta ~ tratamento` para delineamento inteiramente casualizado e produz `models/fit`. A saída é `models/fit`.

## Quando usar

Use quando tratamentos foram casualizados sem restrições entre unidades experimentais.

## Configuração

Informe resposta numérica e tratamento; o tratamento é convertido em fator.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados")
```

`PlantGrowth` contém três grupos independentes de plantas e peso seco como resposta.

## Como interpretar

O F do tratamento compara variação entre médias com erro residual. O desenho pressupõe casualização sem blocos.
