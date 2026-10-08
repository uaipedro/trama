---
title: "Tabela para séries"
description: "Monta uma série múltipla a partir de várias colunas de valores e, opcionalmente, uma de tempo."
section: colecoes
collection: series-temporais
node: series/from_table_mts
category: "Fonte"
related: [series/join, series/pick]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Transforma várias colunas de uma tabela numa série múltipla, uma série por
coluna, todas no mesmo calendário. As regras de tempo são as do
`series/from_table`: tempo repetido ou com buraco é recusado.

## Parâmetros

- **Valores** — as colunas numéricas, uma por série (duas ou mais).
- **Tempo**, **Frequência**, **Início** — como no `series/from_table`.

## Valor

Uma série múltipla (`series/mts`).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("a", "b")) |>
  tr_add("tab", "data/select", cols = "tempo, dax, cac", from = "j") |>
  tr_add("de_volta", "series/from_table_mts", valores = c("dax", "cac"), tempo = "tempo",
         frequencia = 260L, from = "tab")
```

## Veja também

`series/join` para juntar séries que já existem; `series/pick` para tirar uma.

