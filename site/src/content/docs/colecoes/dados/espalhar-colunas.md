---
title: Espalhar colunas
description: Crie colunas a partir dos valores de uma variável de nomes.
section: colecoes
collection: dados
node: data/pivot_wider
category: reformatar
order: 24
related: [data/pivot_longer, data/group_summarise]
---

## O que o bloco faz

Cria uma coluna de saída para cada valor da variável escolhida em **Nomes vêm de**, preenchendo-a com os valores de outra coluna.

## Quando usar

Quando uma tabela longa deve apresentar categorias como colunas, por exemplo um valor por mês.

## Configuração

**Nomes vêm de** (`names_from`) escolhe os valores que criarão colunas; **Valores vêm de** (`values_from`) escolhe o conteúdo; **Preencher vazio com** (`values_fill`) é opcional.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("mensal", "data/generate", n = 4L,
         expr = "produto = rep(c('A', 'B'), each = 2), mes = rep(c('jan', 'fev'), 2), vendas = c(10, 12, 8, 11)") |>
  tr_add("largo", "data/pivot_wider", names_from = "mes", values_from = "vendas",
         from = "mensal")
```

## Como interpretar

A saída volta a ter uma linha por produto e cria as colunas `jan` e `fev`. Os valores de `vendas` ocupam a coluna do mês correspondente.
