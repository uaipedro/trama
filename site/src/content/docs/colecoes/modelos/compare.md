---
title: Comparar modelos
description: "Dois modelos aninhados: os termos a mais melhoram o ajuste? (F ou razão de verossimilhança)"
section: colecoes
collection: modelos
node: models/compare
category: resumir
related: [models/anova_table, models/coefficients]
---

## O que o bloco faz

`models/compare` Compara dois modelos aninhados por teste F ou razão de verossimilhanças. A saída é `models/test`.

## Quando usar

Use quando dois modelos aninhados diferem por um ou mais termos e a pergunta é se esses termos melhoram o ajuste.

## Configuração

Conecte os modelos menor e maior, ajustados à mesma resposta e às mesmas linhas.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("m1", "models/lm", formula = "mpg ~ wt", from = "dados") |>
  tr_add("m2", "models/lm", formula = "mpg ~ wt + hp", from = "dados") |>
  tr_add("resultado", "models/compare", from = c("m1", "m2"))
```

O segundo modelo acrescenta `hp` ao primeiro; ambos usam as mesmas linhas de `mtcars`.

## Como interpretar

P-valor pequeno indica que os termos adicionais melhoram o ajuste sob as suposições do teste. Modelos mistos são reajustados por ML quando necessário.
