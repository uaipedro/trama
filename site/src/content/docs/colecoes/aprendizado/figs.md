---
section: colecoes
title: FIGS · soma de árvores
description: Ajusta uma soma de árvores pequenas sob um orçamento global de divisões para regressão ou classificação binária.
collection: aprendizado
node: ml/figs
related: ["ml/split", "ml/predict", "ml/rules", "ml/tree_plot"]
---

## O que o bloco faz

Ajusta uma soma de árvores pequenas sob um orçamento global de divisões para regressão ou classificação binária.

## Quando usar

Use quando uma explicação aditiva por árvores pequenas atende à pergunta. Para classificação, o suporte é binário. Requer `figsr`.

## Configuração

`alvo`, `cols` e `tarefa` definem dados e tarefa. `max_splits` limita o total de divisões da soma; `min_n` controla o tamanho mínimo dos nós conforme figsr; `seed` reproduz o ajuste.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
m <- trama.ml::tr_ml_figs(d, alvo = "Species", max_splits = 4)
trama.ml::tr_ml_rules(m)
```

## Como interpretar

As regras mostram contribuições por árvore; some as contribuições para obter a previsão. `ml/tree_plot` desenha uma árvore selecionada, enquanto a saída final permanece a soma.
