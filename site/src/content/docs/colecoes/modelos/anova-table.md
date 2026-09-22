---
title: Quadro da ANOVA
description: "O quadro da análise de variância, com SQ tipo I, II ou III e a régua do p-valor por termo."
section: colecoes
collection: modelos
node: models/anova_table
category: resumir
related: [models/coefficients, models/fit_stats]
---

## O que o bloco faz

`models/anova_table` Monta o quadro de efeitos com graus de liberdade, somas de quadrados, estatísticas e p-valores. A saída é `models/effects`.

## Quando usar

Use para inspecionar o teste global de cada termo do ajuste e escolher como as somas de quadrados respondem ao desbalanceamento.

## Configuração

Soma de quadrados aceita I, II ou III. Tipo I é sequencial; II ajusta pelos outros efeitos principais; III inclui todos os termos.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("resultado", "models/anova_table", tipo_sq = "I", from = "ajuste")
```

A entrada é o ajuste DBC de produção: cada linha do quadro apresenta para tratamento e bloco os graus de liberdade, SQ tipo I, F e p-valor.

## Como interpretar

Os tipos coincidem no desenho balanceado. Em dados desbalanceados, escolha o tipo conforme a hipótese e a ordem do modelo.
