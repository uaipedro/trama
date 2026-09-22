---
title: Bartlett
description: "Bartlett: as variâncias dos resíduos são iguais entre os tratamentos?"
section: colecoes
collection: modelos
node: models/bartlett
category: pressupostos
related: [models/breusch_pagan, models/levene]
---

## O que o bloco faz

`models/bartlett` Testa igualdade de variâncias residuais entre grupos. A saída é `models/test`.

## Quando usar

Use para testar igualdade de variâncias entre grupos quando a normalidade residual é plausível.

## Configuração

Não há parâmetros adicionais.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados") |>
  tr_add("resultado", "models/bartlett", from = "ajuste")
```

O teste recebe os resíduos do DIC de `PlantGrowth` e compara suas variâncias nos três grupos; o resultado informa qui-quadrado e p-valor.

## Como interpretar

A hipótese nula é variâncias iguais. Bartlett é sensível à não normalidade; confira os resíduos.
