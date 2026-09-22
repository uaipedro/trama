---
title: Comparações de médias
description: "Todas as diferenças entre pares, ou cada tratamento contra um controle (Dunnett), com p-valor ajustado."
section: colecoes
collection: modelos
node: models/pairwise
category: medias
related: [models/emmeans, models/linear_hypothesis]
---

## O que o bloco faz

`models/pairwise` Compara médias ajustadas em pares ou contra um controle, com p-valores ajustados. A saída é `models/effects`.

## Quando usar

Use após calcular médias ajustadas, quando a pergunta pede contrastes par a par ou comparações com um controle.

## Configuração

Comparar seleciona todos os pares ou contra controle; informe o rótulo do controle quando aplicável e escolha Ajuste.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("medias", "models/emmeans", especs = "hibrido", from = "ajuste") |>
  tr_add("resultado", "models/pairwise", from = "medias")
```

O contraste final compara todos os pares de híbridos com ajuste de Tukey padrão.

## Como interpretar

A saída identifica contraste, diferença e p-valor ajustado. Com Dunnett, cada nível é comparado ao controle indicado.
