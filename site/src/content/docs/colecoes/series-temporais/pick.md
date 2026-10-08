---
title: "Escolher série"
description: "Tira uma série de uma série múltipla, para os blocos univariados."
section: colecoes
collection: series-temporais
node: series/pick
category: "Multivariada"
related: [series/join]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Devolve uma das séries de uma série múltipla, para testar raiz unitária,
decompor ou ajustar um ARIMA nela.

## Parâmetros

- **Série** — o nome da coluna.

## Valor

Uma série (`series/ts`).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("a", "b")) |>
  tr_add("cac", "series/pick", variavel = "cac", from = "j")
```

## Veja também

`series/join`.

