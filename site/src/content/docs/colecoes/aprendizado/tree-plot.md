---
section: colecoes
title: Visualizar árvores
description: Desenha uma árvore CART completa ou uma árvore escolhida da soma FIGS.
collection: aprendizado
node: ml/tree_plot
related: ["ml/cart", "ml/figs", "ml/rules"]
---

## O que o bloco faz

Desenha uma árvore CART completa ou uma árvore escolhida da soma FIGS.

## Quando usar

Use para acompanhar limiares, ramos e valores previstos/contribuições.

## Configuração

`arvore` seleciona índice FIGS (CART ignora); `mostrar_n` inclui número de observações; `mostrar_impureza` inclui impureza/ganho; `casas` controla casas decimais. Recebe um ajuste CART ou FIGS.

## Exemplo

```r
m <- trama.ml::tr_ml_cart(mtcars, "mpg", "wt, hp")
trama.ml::tr_ml_tree_plot(m, mostrar_n = TRUE)
```

## Como interpretar

Nos terminais CART, o rótulo é previsão. No FIGS, é contribuição da árvore selecionada; a previsão total combina as árvores.
