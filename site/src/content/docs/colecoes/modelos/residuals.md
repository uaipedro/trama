---
title: Resíduos
description: "A tabela do ajuste com os valores ajustados, os resíduos e os resíduos padronizados."
section: colecoes
collection: modelos
node: models/residuals
category: resumir
related: [models/anova_table, models/coefficients]
---

## O que o bloco faz

`models/residuals` Produz dados do ajuste com valores ajustados, resíduos e resíduos padronizados. A saída é uma tabela `data/table`.

## Quando usar

Use para localizar observações com resíduos grandes e encaminhá-las à investigação do ajuste.

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
  tr_add("resultado", "models/residuals", from = "ajuste")
```

A tabela do DIC contém as linhas de `PlantGrowth`, o valor ajustado por grupo, o resíduo observado menos ajustado e seu valor padronizado.

## Como interpretar

Resíduo é observado menos ajustado. A tabela contém apenas linhas usadas no ajuste; valores padronizados ajudam a localizar extremos.
