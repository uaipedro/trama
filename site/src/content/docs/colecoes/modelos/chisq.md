---
title: Qui-quadrado
description: "Qui-quadrado de independência entre duas colunas categóricas."
section: colecoes
collection: modelos
node: models/chisq
category: testes
related: [models/cor_test, models/fisher_exact]
---

## O que o bloco faz

`models/chisq` Testa independência entre duas colunas categóricas pelo qui-quadrado. A saída é `data/test`.

## Quando usar

Use para verificar associação entre duas variáveis categóricas em uma tabela de contingência.

## Configuração

Informe Linha e Coluna; Correção de Yates é aplicável a tabelas 2 × 2.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "warpbreaks") |>
  tr_add("resultado", "models/chisq", linha = "wool", coluna = "tension", from = "dados")
```

`warpbreaks` cruza tipo de lã e tensão em uma tabela categórica.

## Como interpretar

A tabela cruza frequências observadas e esperadas. Frequências esperadas pequenas enfraquecem a aproximação.
