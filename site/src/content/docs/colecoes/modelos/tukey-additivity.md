---
title: Aditividade de Tukey
description: "Teste de não aditividade de Tukey: bloco e tratamento interagem?"
section: colecoes
collection: modelos
node: models/tukey_additivity
category: pressupostos
related: [models/bartlett, models/breusch_pagan]
---

## O que o bloco faz

`models/tukey_additivity` Testa aditividade entre bloco e tratamento. A saída é `data/test`.

## Quando usar

Use em DBC para avaliar se tratamento e bloco atuam aditivamente.

## Configuração

Não há parâmetros adicionais; requer ajuste de blocos casualizados.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("resultado", "models/tukey_additivity", from = "ajuste")
```

No DBC simulado, o teste verifica o termo de não aditividade entre `hibrido` e `bloco`; o resultado fornece estatística e p-valor para essa hipótese.

## Como interpretar

A hipótese nula é ausência de interação bloco × tratamento.
