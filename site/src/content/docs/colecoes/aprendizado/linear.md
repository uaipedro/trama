---
section: colecoes
title: Linear / logística
description: Ajusta mínimos quadrados para resposta numérica ou regressão logística binária para resposta categórica de duas classes.
collection: aprendizado
node: ml/linear
related: ["ml/split", "models/predict", "models/evaluate"]
---

## O que o bloco faz

Ajusta mínimos quadrados para resposta numérica ou regressão logística binária para resposta categórica de duas classes.

## Quando usar

Use como referência de comparação quando uma relação linear ou uma fronteira logística for adequada.

## Configuração

`resposta` é a coluna a prever; `preditores` lista preditores numéricos separados por vírgula (vazio usa todos os numéricos exceto a resposta); `tarefa` aceita `auto`, `regressao` ou `classificacao`; `seed` fixa o ajuste.

## Exemplo

```r
d <- trama.ml::tr_ml_example("mtcars")
m <- trama.ml::tr_ml_linear(d, resposta = "mpg", preditores = "wt, hp")
trama.models::tr_models_predict(m, d[1:3, ])
```

## Como interpretar

O modelo produz `models/fit`, consumido por `models/predict` (e pelos avaliadores da coleção de modelos). Resposta numérica em `auto` indica regressão; resposta categórica indica classificação binária.
