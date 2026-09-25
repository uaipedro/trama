---
title: Gráfico de regressão
description: "Pontos, curva ajustada, equação e R²: a figura da dose-resposta ou do modelo não linear."
section: colecoes
collection: modelos
node: models/plot_regression
category: resumir
related: [models/dose_response, models/plot_means]
---

## O que o bloco faz

`models/plot_regression` desenha a curva ajustada por `models/dose_response` ou a regressão não linear com os pontos, a equação e o R². A saída é `view/plot`.

## Quando usar

Use para a figura de regressão de uma tese: médias por dose com a parábola e a dose de máxima eficiência técnica, ou uma curva de crescimento logística com os pontos observados.

## Configuração

Parcelas ao fundo acrescenta, na dose-resposta, cada parcela em cinza atrás das médias. Equação e R² escreve a equação no canto do gráfico, com vírgula decimal. Aspecto, tema, título e rótulos ajustam o gráfico.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "adubo_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "dose", bloco = "bloco", from = "dados") |>
  tr_add("curva", "models/dose_response", tratamento = "dose", from = "ajuste") |>
  tr_add("resultado", "models/plot_regression", titulo = "Produção por dose de N", from = "curva")
```

O gráfico mostra as médias das cinco doses, a parábola ajustada, a equação com R² e uma linha tracejada na dose de máxima eficiência técnica.

## Como interpretar

Na dose-resposta os pontos são médias, porque a curva foi ajustada a elas, e o R² diz quanto da diferença entre as doses a curva explica. No não linear os pontos são as observações e o R² é um pseudo R² (1 − SQ do resíduo / SQ total). No linear-platô a linha tracejada marca o início do platô.
