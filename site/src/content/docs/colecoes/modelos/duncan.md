---
title: Duncan
description: "Teste de Duncan (amplitude múltipla): letras de agrupamento das médias."
section: colecoes
collection: modelos
node: models/duncan
category: medias
related: [models/emmeans, models/linear_hypothesis]
---

## O que o bloco faz

`models/duncan` Aplica o teste de Duncan às médias do tratamento e retorna grupos por letras. A saída é `models/emm`.

## Quando usar

Use em experimentos de tratamento quando o plano analítico especifica o procedimento de Duncan para agrupar médias.

## Configuração

Tratamento escolhe o fator; alfa define o nível de decisão.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("resultado", "models/duncan", tratamento = "hibrido", from = "ajuste")
```

O DBC de milho fornece médias de produção dos híbridos; as letras de Duncan indicam quais híbridos o procedimento não separa ao alfa usado.

## Como interpretar

Letras comuns indicam médias não separadas pelo procedimento. Duncan é menos conservador que Tukey no controle de erro familiar.
