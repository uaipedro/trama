---
title: Sair de fluxo
description: Reúna os resultados produzidos na região de fluxo em uma tabela.
section: colecoes
collection: dados
node: data/from_stream
category: fluxo
order: 28
related: [data/to_stream, data/summary]
---

## O que o bloco faz

Reúne as tabelas dos pontos da região em uma tabela histórica; opcionalmente acrescenta a coluna `passo` com a posição de cada ponto.

## Quando usar

No fim de uma sequência aberta por `data/to_stream`, quando a análise precisa do resultado tabular agregado.

## Configuração

**Coluna do passo** (`passo`) inclui ou omite a coluna `passo` que identifica a posição de cada lote reunido.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("lotes", "data/to_stream", lote = 30L, from = "ar") |>
  tr_add("dias", "data/filter", expr = "Ozone > 30", from = "lotes") |>
  tr_add("resultado", "data/from_stream", passo = TRUE, from = "dias")
```

## Como interpretar

No exemplo, cada linha do resultado é um dia com `Ozone > 30`; `passo` indica de qual lote ordenado por mês e dia veio. Lotes vazios não acrescentam linhas.
