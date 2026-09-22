---
section: colecoes
title: Visualizar tuning
description: Plota a métrica média por tentativa com o melhor valor acumulado ou relaciona um hiperparâmetro à métrica.
collection: aprendizado
node: ml/tuning_plot
related: ["ml/tune", "ml/evaluate"]
---

## O que o bloco faz

Plota a métrica média por tentativa com o melhor valor acumulado ou relaciona um hiperparâmetro à métrica.

## Quando usar

Use após a busca para inspecionar a variação entre configurações e a evolução do resultado.

## Configuração

`hiperparametro` vazio mostra tentativas e melhor acumulado; preenchido, nomeia uma coluna numérica do histórico, como `max_depth`.

## Exemplo

```r
z <- trama.ml::tr_ml_tune(mtcars, "mpg", modelo = "cart", tentativas = 4, folds = 3)
trama.ml::tr_ml_tuning_plot(z$historico, "max_depth")
```

## Como interpretar

O gráfico de dispersão relaciona configuração e média. A curva de melhor valor é acumulada segundo a direção da métrica: erro é minimizado e medidas de classificação são maximizadas.
