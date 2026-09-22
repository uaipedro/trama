---
section: colecoes
title: Random forest
description: Combina árvores aleatorizadas e agrega suas previsões para regressão ou classificação.
collection: aprendizado
node: ml/forest
related: ["ml/split", "ml/predict", "ml/evaluate", "ml/importance"]
---

## O que o bloco faz

Combina árvores aleatorizadas e agrega suas previsões para regressão ou classificação.

## Quando usar

Use para modelar relações não lineares e interações com um conjunto de árvores. Requer `ranger`.

## Configuração

`trees` define o número de árvores; `mtry` define preditores candidatos por divisão (0 usa piso da raiz quadrada do número de preditores); `min_n` é o tamanho mínimo do nó a dividir; `max_depth` limita profundidade. Também recebe `alvo`, `cols`, `tarefa` e `seed`.

## Exemplo

```r
d <- trama.ml::tr_ml_example("mtcars")
m <- trama.ml::tr_ml_forest(d, alvo = "mpg", cols = "wt, hp, disp", trees = 100)
trama.ml::tr_ml_predict(m, d[1:3, ])
```

## Como interpretar

A floresta retorna um ajuste agregado. Importância resume o motor; não é regra individual nem medida causal.
