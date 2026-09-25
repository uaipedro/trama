---
section: colecoes
title: Prever
description: Aplica um ajuste a novas linhas e devolve as colunas recebidas, na mesma ordem, acrescidas de `.pred` e probabilidades quando disponíveis.
collection: aprendizado
node: ml/predict
related: ["ml/split", "ml/evaluate", "ml/confusion", "ml/roc"]
---

## O que o bloco faz

Aplica um ajuste a novas linhas e devolve as colunas recebidas, na mesma ordem, acrescidas de `.pred` e probabilidades quando disponíveis.

## Quando usar

Use com o teste reservado ou com dados novos contendo os mesmos preditores usados no ajuste. A resposta não é necessária.

## Configuração

Recebe `modelo` produzido por um nó de ajuste ou por `ml/tune` e `dados` a prever.

## Exemplo

```r
d <- trama.ml::tr_ml_example("mtcars")
s <- trama.ml::tr_ml_split(d, "mpg")
m <- trama.ml::tr_ml_cart(s$treino, "mpg", "wt, hp")
p <- trama.ml::tr_ml_predict(m, s$teste)
head(p)
```

## Como interpretar

`.pred` contém valor ou classe prevista. Em classificação, cada probabilidade disponível usa o nome `.prob_<classe>`.

### Proveniência

A marca `treino`/`teste` do [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/) passa para a saída, para que a avaliação saiba de onde vieram as linhas. Um modelo ajustado no treino de uma divisão não prevê o teste de outra divisão (`tr_ml_error_split_mismatch`): parte daquelas linhas pode ter estado no treino.
