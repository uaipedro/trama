---
title: t pareado
description: "t pareado: a média das diferenças entre duas medidas na mesma unidade é zero?"
section: colecoes
collection: modelos
node: models/paired_t
category: testes
related: [models/chisq, models/cor_test]
---

## O que o bloco faz

`models/paired_t` Testa se a média das diferenças entre duas medidas pareadas é zero. A saída é `models/test`.

## Quando usar

Use quando cada unidade fornece duas medidas correspondentes, como antes e depois de uma intervenção.

## Configuração

Informe as colunas primeira e segunda medida, observadas na mesma unidade e ordem; escolha a alternativa.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("resultado", "models/paired_t", antes = "drat", depois = "wt", from = "dados")
```

Cada linha de `mtcars` forma um par: `drat` e `wt` são comparados dentro do mesmo carro; o nó retorna a média das diferenças, intervalo e teste t.

## Como interpretar

O cálculo usa diferenças por unidade; não trate as colunas como grupos independentes.
