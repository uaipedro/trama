---
title: ANOVA · DBC
description: "Análise de variância de um delineamento em blocos casualizados."
section: colecoes
collection: modelos
node: models/anova_dbc
category: anova
related: [models/emmeans, models/tukey_additivity, models/polinomial]
---

## O que o bloco faz

`models/anova_dbc` Ajusta `resposta ~ bloco + tratamento` e produz `models/fit`. A saída é `models/fit`.

## Quando usar

Use quando cada bloco recebe os tratamentos e controla uma fonte conhecida de variação entre unidades.

## Configuração

Informe resposta, tratamento e bloco. O tratamento é convertido em fator.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados")
```

O exemplo usa o conjunto simulado `milho_dbc`; a construção planta H3 acima dos demais híbridos.

## Como interpretar

O efeito do tratamento é testado contra o resíduo após ajustar blocos. O modelo supõe aditividade: tratamento tem o mesmo efeito em cada bloco.
