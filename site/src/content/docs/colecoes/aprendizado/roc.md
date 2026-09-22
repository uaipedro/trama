---
section: colecoes
title: Curva ROC
description: Traça sensibilidade contra taxa de falsos positivos para classificação binária e calcula AUC.
collection: aprendizado
node: ml/roc
related: ["ml/predict", "ml/confusion", "ml/evaluate"]
---

## O que o bloco faz

Traça sensibilidade contra taxa de falsos positivos para classificação binária e calcula AUC.

## Quando usar

Use as probabilidades de uma classe para comparar a discriminação ao longo de limiares possíveis.

## Configuração

`alvo` é a classe observada; `probabilidade` é coluna `.prob_<classe>`; `positiva` escolhe a classe positiva (vazia usa a segunda classe observada).

## Exemplo

```r
d <- data.frame(y = factor(c("nao", "sim", "nao", "sim")), .prob_sim = c(.1, .8, .4, .7))
trama.ml::tr_ml_roc(d, "y", ".prob_sim", "sim")
```

## Como interpretar

AUC é a área sob a curva, incluída no gráfico. Ela resume ordenação das classes e deve ser lida junto à distribuição de classes e ao custo das decisões.
