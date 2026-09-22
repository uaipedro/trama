---
title: Regressão linear
description: "Ajusta um modelo linear (lm) pelas colunas ou por uma fórmula digitada."
section: colecoes
collection: modelos
node: models/lm
category: ajustar
related: [models/coefficients, models/plot_diagnostics]
---

## O que o bloco faz

`models/lm` Ajusta uma regressão por mínimos quadrados e produz `models/fit`. A saída é `models/fit`.

## Quando usar

Use quando a resposta contínua e os preditores puderem ser descritos por uma combinação linear; use a fórmula para interações, curvatura ou transformações.

## Configuração

Use resposta e preditores para efeitos principais; preencha Fórmula para interações, transformações ou codificação explícita de fatores. Fórmula prevalece sobre as colunas.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("ajuste", "models/lm", formula = "mpg ~ wt + hp", from = "dados")
```

`mtcars` oferece consumo `mpg` e preditores `wt` e `hp`.

## Como interpretar

R² e R² ajustado resumem variação explicada; o teste F global avalia preditores em conjunto. Coeficientes dependem das unidades e dos contrastes dos fatores.
