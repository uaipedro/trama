---
section: colecoes
title: Matriz de confusão
description: Conta combinações entre classe observada e prevista e devolve colunas `observado`, `previsto` e `n`.
collection: aprendizado
node: ml/confusion
related: ["ml/predict", "ml/evaluate"]
---

## O que o bloco faz

Conta combinações entre classe observada e prevista e devolve colunas `observado`, `previsto` e `n`.

## Quando usar

Use para localizar quais classes se confundem e interpretar métricas agregadas de classificação.

## Configuração

`resposta` nomeia a classe observada e `predito` a classe prevista (padrão `.pred`).

## Exemplo

```r
d <- data.frame(y = c("a", "a", "b"), .pred = c("a", "b", "b"))
trama.ml::tr_ml_confusion(d, resposta = "y")
```

## Como interpretar

Cada linha é um par de classes e sua contagem; combinações sem ocorrências também aparecem. Leia as contagens junto ao tamanho de cada classe.
