---
title: Importância
description: "Quanto cada preditora pesa no modelo, da maior para a menor."
section: colecoes
collection: modelos
node: models/importance
category: resumir
related: [models/coefficients]
---

## O que o bloco faz

`models/importance` devolve uma tabela `termo`, `importancia`, `medida`, ordenada. A medida depende do modelo: |t| (ou |z|) dos coeficientes no `lm` e no GLM, redução de impureza nas árvores de machine learning.

## Quando usar

Use para ordenar preditoras de um mesmo modelo. Não compare valores de medidas diferentes.

## Configuração

Sem parâmetros. O misto e a parcela subdividida recusam.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt + hp + qsec", from = "dados") |>
  tr_add("imp", "models/importance", from = "reg")
```

## Como interpretar

Importância não indica sinal nem causa, e preditoras correlacionadas repartem a importância entre si.
