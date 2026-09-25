---
title: Breusch-Pagan
description: "Breusch-Pagan (Koenker): a variância dos resíduos depende dos preditores?"
section: colecoes
collection: modelos
node: models/breusch_pagan
category: pressupostos
related: [models/bartlett, models/levene]
---

## O que o bloco faz

`models/breusch_pagan` Testa se a variância residual depende dos preditores. A saída é `data/test`.

## Quando usar

Use quando preditores contínuos podem explicar mudanças na variância residual.

## Configuração

Não há parâmetros adicionais.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("ajuste", "models/lm", formula = "mpg ~ wt", from = "dados") |>
  tr_add("resultado", "models/breusch_pagan", from = "ajuste")
```

No ajuste `mpg ~ wt` de `mtcars`, o teste avalia se a variância residual muda com o peso do carro e reporta estatística e p-valor.

## Como interpretar

A hipótese nula é variância constante. Rejeição indica dependência da dispersão, mas não escolhe uma correção.
