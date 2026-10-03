---
title: Medidas de ajuste
description: "R², R² ajustado, R² marginal e condicional, CV, AIC, BIC e log-verossimilhança, em colunas fixas."
section: colecoes
collection: modelos
node: models/fit_stats
category: resumir
related: [models/anova_table, models/coefficients]
---

## O que o bloco faz

`models/fit_stats` Reúne medidas de ajuste compatíveis com o modelo em colunas fixas. A saída é uma tabela `data/table`.

## Quando usar

Use para resumir capacidade de ajuste ou comparar especificações ajustadas à mesma resposta e aos mesmos dados.

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
  tr_add("resultado", "models/fit_stats", from = "ajuste")
```

Para o ajuste `weight ~ group`, a tabela apresenta R², R² ajustado, AIC, BIC e log-verossimilhança quando definidos para esse modelo linear.

## Como interpretar

AIC e BIC são comparáveis entre modelos ajustados à mesma resposta e aos mesmos dados. Em mistos, R² marginal e condicional referem-se a parcelas diferentes da variação.
