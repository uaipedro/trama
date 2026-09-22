---
title: Empilhar colunas
description: Transforme várias colunas em uma coluna de nomes e outra de valores.
section: colecoes
collection: dados
node: data/pivot_longer
category: reformatar
order: 23
related: [data/pivot_wider, data/group_summarise]
---

## O que o bloco faz

Converte cada coluna selecionada em observações de uma coluna de nomes e outra de valores, mantendo as demais colunas como identificadores.

## Quando usar

Quando categorias estão representadas em colunas e devem virar valores de uma variável, por exemplo meses em cabeçalhos.

## Configuração

**Colunas** (`cols`) seleciona os campos que serão empilhados; **Nome vai para** (`names_to`) e **Valores vão para** (`values_to`) nomeiam as duas colunas resultantes.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("largos", "data/generate", n = 3L,
         expr = "tibble::tibble(produto = c('A', 'B', 'C'), jan = c(10, 12, 8), fev = c(11, 9, 13), mar = c(14, 15, 10))") |>
  tr_add("mensal", "data/pivot_longer", cols = "jan, fev, mar",
         names_to = "mes", values_to = "vendas", from = "largos")
```

## Como interpretar

Três produtos multiplicados por três meses produzem nove linhas. `mes` guarda `jan`, `fev` ou `mar`; `vendas` guarda o valor correspondente.
