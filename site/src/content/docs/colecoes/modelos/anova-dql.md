---
title: ANOVA · DQL
description: "Análise de variância de um delineamento em quadrado latino."
section: colecoes
collection: modelos
node: models/anova_dql
category: anova
related: [models/emmeans, models/anova_dbc]
---

## O que o bloco faz

`models/anova_dql` Ajusta `resposta ~ linha + coluna + tratamento` para quadrado latino. A saída é `models/fit`.

## Quando usar

Use quando há duas fontes cruzadas de heterogeneidade e cada tratamento aparece uma vez em cada linha e coluna.

## Configuração

Informe resposta, tratamento, linha e coluna; cada dimensão precisa ter o mesmo número de níveis.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "racao_dql") |>
  tr_add("ajuste", "models/anova_dql", resposta = "ganho_peso", tratamento = "racao", linha = "periodo", coluna = "lote", from = "dados")
```

O conjunto `racao_dql` contém um quadrado latino 5 × 5 com ganho de peso como resposta.

## Como interpretar

O quadro separa os efeitos de linha, coluna e tratamento. A disposição deve conter uma observação por célula do quadrado.
