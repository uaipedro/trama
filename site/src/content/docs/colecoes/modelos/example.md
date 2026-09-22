---
title: Exemplo de modelos
description: "Carrega um conjunto de dados escolhido para ensinar um modelo ou delineamento."
section: colecoes
collection: modelos
node: models/example
category: fonte
related: [models/anova_dic, models/lm]
---

## O que o bloco faz

`models/example` Carrega o conjunto selecionado como tabela. A saída é uma tabela `data/table`. A saída é `data/table`.

## Quando usar

Use este bloco para experimentar uma técnica antes de conectá-la aos seus próprios dados; escolha o conjunto correspondente ao desenho ou à família de resposta.

## Configuração

Escolha o conjunto no seletor; os conjuntos simulados `milho_dbc` e `racao_dql` têm semente fixa e efeitos plantados.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth")
```

A saída é a tabela `PlantGrowth` com 30 linhas e as colunas `weight` e `group` (controle e dois tratamentos), pronta para conectar a um ajuste.

## Como interpretar

Cada conjunto tem colunas documentadas no seletor; os simulados permitem conferir se os efeitos conhecidos reaparecem.
