---
section: colecoes
title: CART · árvore de decisão
description: Ajusta uma árvore de decisão para regressão ou classificação; cada caminho até uma folha corresponde a uma regra.
collection: aprendizado
node: ml/cart
related: ["ml/split", "ml/predict", "ml/rules", "ml/tree_plot", "ml/importance"]
---

## O que o bloco faz

Ajusta uma árvore de decisão para regressão ou classificação; cada caminho até uma folha corresponde a uma regra.

## Quando usar

Use quando quiser inspecionar decisões locais por limiares e folhas.

## Configuração

`alvo`, `cols` e `tarefa` definem a resposta, preditores numéricos e tipo de tarefa. `max_depth` limita a profundidade; `min_n` define o mínimo de observações por folha; `seed` reproduz o ajuste. Requer `rpart`.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
m <- trama.ml::tr_ml_cart(d, alvo = "Species", cols = "Petal.Length, Petal.Width", max_depth = 3)
trama.ml::tr_ml_rules(m)
```

## Como interpretar

Cada linha de regras representa uma folha e seu valor previsto. `ml/tree_plot` mostra a estrutura e `ml/importance` resume importância do motor.
