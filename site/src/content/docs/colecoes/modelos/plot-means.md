---
title: Gráfico de médias
description: "Médias com intervalo de confiança e as letras de comparação."
section: colecoes
collection: modelos
node: models/plot_means
category: medias
related: [models/duncan, models/emmeans, models/scott_knott]
---

## O que o bloco faz

`models/plot_means` representa médias com intervalos e, opcionalmente, letras. A saída é `view/plot`. O eixo diz a origem: "média ajustada" quando vêm do `models/emmeans`, "média" quando são as da tabela (Duncan, Waller-Duncan, Scott-Knott), sempre com o nível de confiança pedido.

## Quando usar

Use para apresentar médias, seus intervalos e agrupamentos de comparação em uma figura.

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

Pontos são as médias (ajustadas ou da tabela, conforme o eixo); barras são intervalos. Letras compartilhadas indicam ausência de diferença detectada pelo ajuste escolhido.
