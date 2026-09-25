---
title: t para uma amostra
description: "t para uma amostra: a média da coluna é igual a um valor de referência?"
section: colecoes
collection: modelos
node: models/one_sample_t
category: testes
related: [models/chisq, models/cor_test]
---

## O que o bloco faz

`models/one_sample_t` Compara a média de uma coluna com valor de referência. A saída é `data/test`.

## Quando usar

Use para comparar uma média amostral a um valor de referência definido previamente.

## Configuração

Informe Variável, mu e alternativa bilateral, menor ou maior.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("resultado", "models/one_sample_t", variavel = "weight", mu = 5, from = "dados")
```

Com `PlantGrowth`, o bloco lê `weight` e calcula a diferença entre a média amostral e `mu = 5`, com estatística t, p-valor e intervalo para essa diferença.

## Como interpretar

O intervalo e o teste referem-se à média populacional frente ao valor mu.
