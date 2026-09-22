---
title: Efeitos aleatórios
description: "Os componentes de variância de um modelo misto, com a proporção de cada um."
section: colecoes
collection: modelos
node: models/random_effects
category: resumir
related: [models/anova_table, models/coefficients]
---

## O que o bloco faz

`models/random_effects` Extrai componentes de variância e proporções dos efeitos aleatórios. A saída é uma tabela `data/table`.

## Quando usar

Use para quantificar quanto a resposta varia entre os níveis dos termos aleatórios de um modelo misto.

## Configuração

Não há parâmetros adicionais; a entrada deve ser modelo misto.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "sleepstudy") |>
  tr_add("ajuste", "models/lmer", formula = "Reaction ~ Days + (Days | Subject)", from = "dados") |>
  tr_add("resultado", "models/random_effects", from = "ajuste")
```

No modelo de `sleepstudy`, a tabela discrimina variâncias do intercepto e da inclinação de `Days` entre sujeitos, além da correlação e da variância residual.

## Como interpretar

Variância descreve dispersão entre níveis do grupo; proporções dependem da estrutura aleatória especificada.
