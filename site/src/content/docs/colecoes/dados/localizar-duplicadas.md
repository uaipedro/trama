---
title: Localizar duplicadas
description: Mostre linhas repetidas por uma ou mais colunas-chave e conte suas ocorrências.
section: colecoes
collection: dados
node: data/get_dupes
category: conhecer
order: 10
related: [data/distinct, data/join]
---

## O que o bloco faz

Agrupa as linhas pela combinação de colunas escolhida e devolve as combinações repetidas com o número de ocorrências.

## Quando usar

Antes de remover repetições ou juntar tabelas, quando é necessário verificar quais chaves se repetem.

## Configuração

**Colunas** (`cols`) recebe os campos cuja combinação forma a chave a verificar. Vazio significa comparar todas as colunas.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ensaios", "data/generate", n = 6L,
         expr = "lote = rep(c('A', 'A', 'B'), 2), parcela = rep(c(1, 1, 2), 2), valor = c(12, 12, 15, 13, 13, 17)") |>
  tr_add("repetidas", "data/get_dupes", cols = "lote, parcela", from = "ensaios")
```

## Como interpretar

Neste conjunto, `(A, 1)` aparece quatro vezes e `(B, 2)` duas vezes; as demais combinações não entram no resultado. `dupe_count` informa quantas linhas pertencem a cada combinação repetida.
