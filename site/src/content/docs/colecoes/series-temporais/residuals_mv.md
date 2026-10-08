---
title: "Resíduos (múltiplos)"
description: "Os resíduos do VAR ou VECM, como série múltipla, para olhar com os blocos univariados."
section: colecoes
collection: series-temporais
node: series/residuals_mv
category: "Multivariada"
related: [series/pick, series/portmanteau_mv]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Devolve os resíduos de cada equação do VAR ou do VECM como uma série
múltipla, uma coluna por série, no calendário das observações que têm resíduo
(as `p` primeiras não têm, porque não há passado para elas).

Para olhar um resíduo com os blocos univariados (ACF, ADF, normalidade da
série) passe a saída por `series/pick`.

## Parâmetros

Este bloco não tem parâmetros.

## Valor

Uma série múltipla (`series/mts`) com os resíduos.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", from = c("a", "b")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("res", "series/residuals_mv", from = c(modelo = "v"))
```

## Veja também

`series/pick` para tirar uma coluna; `series/portmanteau_mv` para testar a
autocorrelação destes mesmos resíduos.

