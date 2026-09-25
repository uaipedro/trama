---
section: colecoes
title: XGBoost
description: Ajusta árvores sequencialmente, cada rodada acrescentando correções ao conjunto para regressão ou classificação.
collection: aprendizado
node: ml/xgboost
related: ["ml/split", "ml/predict", "ml/evaluate", "ml/importance", "ml/tune"]
---

## O que o bloco faz

Ajusta árvores sequencialmente, cada rodada acrescentando correções ao conjunto para regressão ou classificação.

## Quando usar

Use para modelos de árvores impulsionadas e compare o resultado no mesmo conjunto de teste. Requer `xgboost`.

## Configuração

`nrounds` define rodadas; `max_depth` limita profundidade por árvore; `eta` controla taxa de aprendizado. Recebe também `alvo`, `cols`, `tarefa` e `seed`.

## Exemplo

```r
d <- trama.ml::tr_ml_example("mtcars")
m <- trama.ml::tr_ml_xgboost(d, alvo = "mpg", cols = "wt, hp", nrounds = 50)
trama.ml::tr_ml_predict(m, d[1:3, ])
```

## Como interpretar

A importância por ganho resume o ajuste e não indica causalidade. O pacote não seleciona rodadas automaticamente; escolha configurações usando treino/validação.

### Teste recusado

A saída `teste` do [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/) é recusada aqui (`tr_ml_error_test_leak`): ajustar nela treinaria no teste. Ligue a saída `treino`; o teste vai só ao `ml/predict`.
