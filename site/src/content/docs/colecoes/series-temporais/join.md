---
title: "Juntar séries"
description: "Junta duas ou mais séries de mesma frequência numa série múltipla, no período comum."
section: colecoes
collection: series-temporais
node: series/join
category: "Multivariada"
related: [series/pick, series/var]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Junta as séries ligadas numa série múltipla — a entrada dos blocos que olham
várias séries ao mesmo tempo (VAR, cointegração, Granger). As séries precisam
ter a mesma frequência; o período é o COMUM a todas, e o card avisa quantas
observações ficaram de fora.

## Parâmetros

- **Nomes** — nomes das séries, separados por vírgula, na ordem dos fios.
  Vazio usa `serie_1`, `serie_2`...

## Valor

Uma série múltipla (`series/mts`).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac"))
```

## Veja também

`series/pick` faz o caminho de volta; `series/var` ajusta o modelo.

