---
section: colecoes
title: Ler regras das árvores
description: Expõe os caminhos e valores das folhas de um modelo CART ou as folhas/contribuições das árvores FIGS.
collection: aprendizado
node: ml/rules
related: ["ml/cart", "ml/figs", "ml/tree_plot", "models/importance"]
---

## O que o bloco faz

Expõe os caminhos e valores das folhas de um modelo CART ou as folhas/contribuições das árvores FIGS.

## Quando usar

Use para exportar e conferir a lógica de CART ou FIGS em tabela.

## Configuração

Recebe `modelo` ajustado por CART ou FIGS.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
m <- trama.ml::tr_ml_cart(d, "Species", "Petal.Length, Petal.Width")
trama.ml::tr_ml_rules(m)
```

## Como interpretar

Em CART, cada linha é uma folha escolhida pelo caminho. Em FIGS, cada árvore contribui para a soma; regras descrevem o ajuste observado.
