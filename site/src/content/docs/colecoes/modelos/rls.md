---
title: RLS online
description: "Regressão linear que aprende ponto a ponto: mínimos quadrados recursivos, dentro de uma região de fluxo."
section: colecoes
collection: modelos
node: models/rls
category: ajustar
related: [models/glm, models/lm]
---

## O que o bloco faz

`models/rls` Atualiza coeficientes ponto a ponto e emite previsão anterior ao ponto, valor observado e resíduo. A saída é um fluxo de tabela com previsto, real e resíduo por ponto.

## Quando usar

Use em região de fluxo quando cada observação precisa atualizar a regressão antes de processar a próxima.

## Configuração

Resposta indica a coluna observada; Preditores lista covariáveis ou fica vazio para usar as demais colunas. Lambda define a incerteza inicial.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "cars") |>
  tr_add("lotes", "data/to_stream", lote = 1, from = "dados") |>
  tr_add("rls", "models/rls", resposta = "dist", preditores = "speed", from = "lotes") |>
  tr_add("historico", "data/from_stream", from = "rls")
```

`data/to_stream` entrega cada carro como ponto; `data/from_stream` recompõe o histórico das previsões.

## Como interpretar

Previsão é calculada antes de incorporar o ponto; resíduo é real menos previsto. Lambda grande representa prior difusa; valor pequeno pode manter viés em direção a zero.
