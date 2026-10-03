---
title: Teste de correlação
description: "Correlação de Pearson, Spearman ou Kendall entre duas colunas, com o teste de que ela é zero."
section: colecoes
collection: modelos
node: models/cor_test
category: testes
related: [models/chisq, models/fisher_exact]
---

## O que o bloco faz

`models/cor_test` Estima e testa correlação entre duas colunas. A saída é `data/test`.

## Quando usar

Use para quantificar associação entre duas medidas numéricas e testar se ela difere de zero.

## Configuração

Informe X e Y numéricos; Método escolhe Pearson, Spearman ou Kendall.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("resultado", "models/cor_test", x = "wt", y = "mpg", metodo = "pearson", from = "dados")
```

`mtcars` relaciona peso do carro (`wt`) ao consumo (`mpg`).

## Como interpretar

Pearson mede associação linear; Spearman e Kendall medem associação monotônica por postos. O sinal indica direção.
