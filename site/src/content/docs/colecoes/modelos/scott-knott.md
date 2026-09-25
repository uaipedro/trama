---
title: Scott-Knott
description: "Teste de Scott-Knott: agrupa as médias em grupos sem sobreposição."
section: colecoes
collection: modelos
node: models/scott_knott
category: medias
related: [models/duncan, models/emmeans, models/plot_means]
---

## O que o bloco faz

`models/scott_knott` parte as médias ordenadas do tratamento em grupos por divisões sucessivas (Scott & Knott, 1974) e retorna uma letra por média. A saída é `models/emm`.

## Quando usar

Use quando há muitos tratamentos (cultivares, linhagens, clones) e se quer grupos que não se sobrepõem: cada média recebe uma letra só, nunca "ab". É o agrupamento mais comum nas revistas brasileiras de ciências agrárias.

## Configuração

Tratamento escolhe o fator; Confiança (padrão 0,95) define o nível de cada corte, com alfa = 1 − confiança. O erro é o resíduo do modelo; na parcela subdividida, o erro (a) para o fator da parcela e o (b) para o da subparcela.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("resultado", "models/scott_knott", tratamento = "hibrido", from = "ajuste")
```

No DBC de milho os cinco híbridos se dividem em três grupos: H3 sozinho no topo, H5 no meio, e H1, H2 e H4 juntos.

## Como interpretar

Médias com a mesma letra estão no mesmo grupo; letras diferentes indicam grupos separados pelo teste. Como os grupos não se sobrepõem, o Scott-Knott pode separar médias próximas que caem em lados diferentes de um corte — leia a letra como grupo, e não como comparação par a par.
