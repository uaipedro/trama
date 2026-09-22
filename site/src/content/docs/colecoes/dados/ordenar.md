---
title: Ordenar
description: Ordene linhas pelos valores de uma ou mais colunas.
section: colecoes
collection: dados
node: data/arrange
category: transformar
order: 21
related: [data/slice_head, data/filter]
---

## O que o bloco faz

Reordena as linhas pelos valores das colunas escolhidas, em ordem crescente ou decrescente.

## Quando usar

Antes de selecionar primeiras linhas ou quando uma saída precisa de ordem explícita.

## Configuração

**Colunas** (`cols`) define a chave e sua ordem. **Decrescente** (`desc`) inverte a direção; o padrão é crescente.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("carros", "data/example", dataset = "mtcars") |>
  tr_add("eficientes", "data/arrange", cols = "mpg", desc = TRUE, from = "carros")
```

## Como interpretar

As 32 linhas permanecem. Os carros com maior `mpg` aparecem primeiro; use `data/slice_head` depois se precisar limitar a saída aos primeiros.
