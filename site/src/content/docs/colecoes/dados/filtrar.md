---
title: Filtrar
description: Retenha linhas cuja expressão R resulta em verdadeiro.
section: colecoes
collection: dados
node: data/filter
category: transformar
order: 18
related: [data/mutate, data/arrange]
---

## O que o bloco faz

Retém as linhas para as quais a condição R retorna `TRUE`; condições falsas ou faltantes não entram na saída.

## Quando usar

Quando a pergunta define uma condição de inclusão, como selecionar vendas acima de um limite.

## Configuração

**Condição** (`expr`) recebe uma expressão R sobre as colunas; **Por grupo** (`by`) opcional define onde a expressão é avaliada separadamente.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("dias", "data/filter", expr = "Month == 7 & Ozone > 30", from = "ar")
```

## Como interpretar

A tabela `dias` reúne apenas observações de julho (`Month == 7`) com `Ozone` acima de 30. A condição que seria falsa ou `NA` não produz linha.
