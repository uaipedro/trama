---
title: Primeiras N
description: Retenha as primeiras linhas da tabela ou de cada grupo.
section: colecoes
collection: dados
node: data/slice_head
category: transformar
order: 22
related: [data/arrange, data/filter]
---

## O que o bloco faz

Retém as primeiras `N` linhas na ordem recebida, ou as primeiras `N` de cada grupo quando **Por grupo** está preenchido.

## Quando usar

Para obter uma amostra inicial ou um top N após ordenar os dados.

## Configuração

**N** (`n`) define o limite, de 1 a 10.000. **Por grupo** (`by`) opcional aplica esse limite dentro de cada grupo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("carros", "data/example", dataset = "mtcars") |>
  tr_add("maiores", "data/arrange", cols = "mpg", desc = TRUE, from = "carros") |>
  tr_add("top5", "data/slice_head", n = 5L, from = "maiores")
```

## Como interpretar

Depois da ordenação decrescente por `mpg`, `top5` contém os cinco carros mais econômicos de `mtcars` segundo essa coluna.
