---
title: Coeficientes
description: "Estimativa, erro padrão, estatística, p-valor e intervalo de 95% de cada coeficiente."
section: colecoes
collection: modelos
node: models/coefficients
category: resumir
related: [models/anova_table, models/compare]
---

## O que o bloco faz

`models/coefficients` Produz estimativa, erro padrão, estatística, p-valor e intervalo de 95% para cada coeficiente. A saída é `models/effects`.

## Quando usar

Use para ler o tamanho e a incerteza de cada coeficiente; use médias ajustadas para comparar níveis de um fator como um conjunto.

## Configuração

Exponenciar transforma estimativas e intervalos; em GLM com ligação log ou logit, resulta em razões de taxas ou chances.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados") |>
  tr_add("resultado", "models/coefficients", exponenciar = FALSE, from = "ajuste")
```

No DIC de `PlantGrowth`, a tabela lista intercepto e diferenças de `trt` e `ctrl` em relação ao nível de referência `ctrl`, com estimativas e incerteza.

## Como interpretar

Cada nível de fator compara com o nível de referência. O p-valor do coeficiente não é teste global do fator.
