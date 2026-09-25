---
title: Kruskal-Wallis
description: "Kruskal-Wallis: dois ou mais grupos vêm da mesma distribuição? (não paramétrico)"
section: colecoes
collection: modelos
node: models/kruskal
category: testes
related: [models/chisq, models/cor_test]
---

## O que o bloco faz

`models/kruskal` Compara dois ou mais grupos pelo teste de Kruskal–Wallis. A saída é `data/test`.

## Quando usar

Use para comparar distribuições em vários grupos independentes usando postos.

## Configuração

Informe resposta numérica e grupo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "InsectSprays") |>
  tr_add("resultado", "models/kruskal", resposta = "count", grupo = "spray", from = "dados")
```

`InsectSprays` tem seis grupos de inseticida e contagens de insetos.

## Como interpretar

A hipótese nula é mesma distribuição. Interpretação como diferença de medianas requer formas semelhantes.
