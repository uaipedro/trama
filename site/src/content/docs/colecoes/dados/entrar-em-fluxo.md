---
title: Entrar em fluxo
description: Divida a tabela em lotes para executar uma região passo a passo.
section: colecoes
collection: dados
node: data/to_stream
category: fluxo
order: 27
related: [data/from_stream, data/filter]
---

## O que o bloco faz

Ordena, se solicitado, e divide a tabela em pontos de até `lote` linhas para abrir uma região processada etapa por etapa.

## Quando usar

Quando o fluxo precisa processar registros por etapas ou expor resultados parciais por lote.

## Configuração

**Linhas por passo** (`lote`) define o tamanho dos lotes; **Ordenar por** (`ordenar_por`) opcional ordena antes do fatiamento; **Máximo de passos** (`max_passos`) limita a sequência, e zero mantém todos os passos.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("lotes", "data/to_stream", lote = 30L,
         ordenar_por = "Month, Day", from = "ar") |>
  tr_add("dias", "data/filter", expr = "Ozone > 30", from = "lotes") |>
  tr_add("resultado", "data/from_stream", passo = TRUE, from = "dias")
```

## Como interpretar

O fluxo ordena `airquality` por mês e dia, cria lotes de até 30 linhas, mantém os dias com `Ozone > 30` em cada ponto e os reúne com a coluna `passo`. Os pontos finais podem ter menos de 30 linhas.
