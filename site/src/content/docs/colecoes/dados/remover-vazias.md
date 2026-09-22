---
title: Remover vazias
description: Retire linhas, colunas ou ambas quando estão inteiramente vazias.
section: colecoes
collection: dados
node: data/remove_empty
category: limpar
order: 12
related: [data/drop_na, data/summary]
---

## O que o bloco faz

Remove linhas, colunas ou ambos quando todos os seus valores são faltantes.

## Quando usar

Para remover margens vazias comuns em planilhas exportadas.

## Configuração

**Remover** (`which`) escolhe `linhas`, `colunas` ou `ambos`; o padrão é `ambos`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("entrada", "data/generate",
         expr = "tibble::tibble(id = c(1, NA, NA), nota = c('ok', NA, NA))") |>
  tr_add("limpa", "data/remove_empty", which = "ambos", from = "entrada")
```

## Como interpretar

As duas linhas que contêm apenas `NA` saem; `id` e `nota` permanecem porque cada coluna tem um valor observado na primeira linha.
