---
title: Scott-Knott
description: "Scott-Knott: separa as médias em grupos sem sobreposição."
section: colecoes
collection: modelos
node: models/scott_knott
category: medias
related: [models/duncan, models/emmeans, models/plot_means]
---

## O que o bloco faz

`models/scott_knott` agrupa as médias de um tratamento pelo método de Scott & Knott (1974): ordena as médias, acha o corte que divide o conjunto em dois grupos com a maior soma de quadrados entre eles, testa esse corte pela razão de verossimilhança e repete dentro de cada lado enquanto o corte for significativo. A saída é `models/emm`, com uma letra por média.

## Quando usar

Para dividir muitos tratamentos em grupos que não se sobrepõem, sobre uma ANOVA balanceada (DIC, DBC, DQL, fatorial, parcela subdividida). O nível vale para cada corte, não para o agrupamento inteiro. Com bloco incompleto, covariável ou repetições desiguais o bloco recusa: as médias da tabela deixam de ter a variância comum que o teste supõe. Nesses casos, use o `models/emmeans`.

## Configuração

Informe o tratamento e a confiança (padrão 0,95; cada corte é testado a alfa = 1 − confiança). Na parcela subdividida, o bloco usa o erro (a) ou (b) do fator.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("dbc", "models/anova_dbc", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho") |>
  tr_add("sk", "models/scott_knott", tratamento = "hibrido", from = "dbc")
```

No `milho_dbc` (QM do resíduo 0,0931 com 12 gl), os cinco híbridos formam três grupos: H3 (9,07) no grupo a, H5 (8,47) no b, e H1, H2 e H4 (7,96, 7,87 e 7,66) no c.

## Como interpretar

Médias com a mesma letra ficaram no mesmo grupo: nenhum corte entre elas foi significativo. Como cada média tem uma letra só, a leitura é mais simples que a do Tukey, mas o método não controla a taxa de erro do experimento inteiro.
