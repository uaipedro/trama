---
title: Wilcoxon-Mann-Whitney
description: "Wilcoxon-Mann-Whitney: dois grupos independentes têm a mesma locação? (não paramétrico)"
section: colecoes
collection: modelos
node: models/wilcoxon
category: testes
related: [models/chisq, models/cor_test]
---

## O que o bloco faz

`models/wilcoxon` Compara localização de dois grupos independentes por postos. A saída é `data/test`.

## Quando usar

Use para comparar dois grupos independentes com escala ordenável quando o teste t não representa bem a pergunta.

## Configuração

Informe resposta, grupo de dois níveis e alternativa.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "ToothGrowth") |>
  tr_add("resultado", "models/wilcoxon", resposta = "len", grupo = "supp", from = "dados")
```

Com `ToothGrowth`, o teste compara por postos os comprimentos `len` dos suplementos `OJ` e `VC`, retornando W e p-valor.

## Como interpretar

A hipótese nula compara distribuições. Interpretação como diferença de medianas requer formas semelhantes.
