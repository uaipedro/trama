---
title: Gráfico de médias
description: "Médias ajustadas com intervalo de confiança e as letras de comparação."
section: colecoes
collection: modelos
node: models/plot_means
category: medias
related: [models/duncan, models/emmeans]
---

## O que o bloco faz

`models/plot_means` Representa médias ajustadas com intervalos e, opcionalmente, letras. A saída é `view/plot`.

## Quando usar

Use para apresentar médias ajustadas, seus intervalos e agrupamentos de comparação em uma figura.

## Configuração

Letras pode ser desligado; aspecto, tema e título ajustam o gráfico.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("medias", "models/emmeans", especs = "hibrido", from = "ajuste") |>
  tr_add("resultado", "models/plot_means", from = "medias")
```

O gráfico representa as médias de produção ajustadas por `hibrido`, com os intervalos calculados por `emmeans` e letras de comparação Tukey.

## Como interpretar

Pontos são médias ajustadas; barras são intervalos. Letras compartilhadas indicam ausência de diferença detectada pelo ajuste escolhido.
