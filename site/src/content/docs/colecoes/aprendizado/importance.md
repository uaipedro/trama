---
section: colecoes
title: Importância de variáveis
description: Ordena a importância interna das variáveis em modelos de árvore.
collection: aprendizado
node: ml/importance
related: ["ml/cart", "ml/figs", "ml/forest", "ml/xgboost", "ml/rules"]
---

## O que o bloco faz

Ordena a importância interna das variáveis em modelos de árvore.

## Quando usar

Use para resumir quais preditores participaram mais do ajuste em CART, FIGS, floresta ou XGBoost.

## Configuração

Recebe `modelo` ajustado por CART, FIGS, random forest ou XGBoost.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
m <- trama.ml::tr_ml_cart(d, "Species", "Petal.Length, Petal.Width")
trama.ml::tr_ml_importance(m)
```

## Como interpretar

A tabela contém `variavel` e `importancia`. As medidas dependem do motor e não são comparáveis entre famílias; variáveis correlacionadas podem repartir importância.
