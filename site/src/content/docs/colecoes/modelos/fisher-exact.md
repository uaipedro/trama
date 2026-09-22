---
title: Exato de Fisher
description: "Teste exato de Fisher: duas colunas categóricas são independentes?"
section: colecoes
collection: modelos
node: models/fisher_exact
category: testes
related: [models/chisq, models/cor_test]
---

## O que o bloco faz

`models/fisher_exact` Testa independência entre colunas categóricas pelo teste exato de Fisher. A saída é `models/test`.

## Quando usar

Use para testar independência em tabela categórica pequena, quando a aproximação qui-quadrado é fraca.

## Configuração

Informe Linha e Coluna categóricas.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("resultado", "models/fisher_exact", linha = "am", coluna = "vs", from = "dados")
```

`mtcars` fornece duas variáveis binárias que formam uma tabela 2 × 2.

## Como interpretar

O cálculo exato é útil em tabelas pequenas; leia a tabela de frequências junto do resultado.
