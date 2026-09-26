---
section: colecoes
title: Random forest
description: Combina árvores aleatorizadas e agrega suas previsões para regressão ou classificação.
collection: aprendizado
node: ml/forest
related: ["ml/split", "models/predict", "models/evaluate", "models/importance"]
---

## O que o bloco faz

Combina árvores aleatorizadas e agrega suas previsões para regressão ou classificação.

## Quando usar

Use para modelar relações não lineares e interações com um conjunto de árvores. Requer `ranger`.

## Configuração

`trees` define o número de árvores; `mtry` define preditores candidatos por divisão (0 usa piso da raiz quadrada do número de preditores); `min_n` é o tamanho mínimo do nó a dividir; `max_depth` limita profundidade; `importancia` escolhe a medida lida no `models/importance` — `impureza` (padrão), `permutacao` ou `impureza_corrigida`. Também recebe `resposta`, `preditores`, `tarefa` e `seed`.

## Exemplo

```r
d <- trama.ml::tr_ml_example("mtcars")
m <- trama.ml::tr_ml_forest(d, resposta = "mpg", preditores = "wt, hp, disp", trees = 100)
trama.models::tr_models_predict(m, d[1:3, ])
```

## Como interpretar

A floresta retorna um ajuste agregado. Importância resume o motor; não é regra individual nem medida causal.

### Teste recusado

A saída `teste` do [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/) é recusada aqui (`tr_ml_error_test_leak`): ajustar nela treinaria no teste. Ligue a saída `treino`; o teste vai só ao `ml/predict`.
