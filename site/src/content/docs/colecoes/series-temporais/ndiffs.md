---
title: Diferenças necessárias
description: "Quantas diferenças simples e sazonais a série pede para ficar estacionária."
section: colecoes
collection: series-temporais
node: series/ndiffs
category: Operar
order: 2
related: [series/diff, series/interpolate, series/window]
---

## O que o bloco faz

Responde, antes de montar o `series/diff` ou de fixar o `d` e o `D` de um
ARIMA à mão, quantas diferenças a série pede.

- **simples**: aplica o teste escolhido em sequência — a série, a diferença, a
  diferença da diferença — até a série passar (`forecast::ndiffs`).
- **sazonal**: pelo teste de força sazonal (`forecast::nsdiffs`). Só existe
  para série com ciclo e dois ciclos de dado; na anual sai em branco, com o
  motivo na nota — e não zero, que afirmaria "testei e não precisa".

A ordem prática: faça as sazonais primeiro, e rode este nó de novo depois delas
para as simples.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Estime quantas diferenças simples e sazonais são necessárias para obter uma série estacionária antes do ajuste. Use a indicação como apoio e confira-a com gráficos e testes.

## Configuração

- **Teste** — o teste de raiz unitária das diferenças simples: `kpss`
  (padrão), `adf` ou `pp`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("quantas", "series/ndiffs", from = "pax")
```

## Como interpretar

Uma tabela (`data/table`) com as linhas `simples` e `sazonal`.

## Veja também

`series/diff` para aplicar; `series/adf` e `series/kpss` para conferir depois.
