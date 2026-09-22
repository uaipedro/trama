---
title: Remover duplicadas
description: Mantenha uma linha por combinação das colunas selecionadas.
section: colecoes
collection: dados
node: data/distinct
category: limpar
order: 13
related: [data/get_dupes, data/arrange]
---

## O que o bloco faz

Mantém a primeira linha de cada combinação distinta nas colunas selecionadas, preservando a ordem da tabela.

## Quando usar

Quando a análise precisa de registros únicos segundo uma chave definida.

## Configuração

**Colunas** (`cols`) recebe os campos que definem duplicidade. A primeira ocorrência na ordem de entrada permanece.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ensaios", "data/generate", n = 4L,
         expr = "parcela = c(1, 1, 2, 2), valor = c(10, 11, 12, 13)") |>
  tr_add("unicas", "data/distinct", cols = "parcela", from = "ensaios")
```

## Como interpretar

A saída mantém as parcelas 1 e 2. Como a entrada está na ordem 1, 1, 2, 2, ficam os valores 10 e 12, respectivamente.
