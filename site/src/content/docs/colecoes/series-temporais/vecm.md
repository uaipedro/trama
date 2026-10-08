---
title: "VECM"
description: "Modelo de correção de erro (VECM) com o posto de cointegração informado."
section: colecoes
collection: series-temporais
node: series/vecm
category: "Multivariada"
related: [series/johansen, series/forecast]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Ajusta um modelo de correção de erro (VECM) para séries cointegradas: as
diferenças de cada série são explicadas pelas diferenças passadas e pelo desvio
da relação de longo prazo (o vetor de cointegração).

O posto r é o número de relações de cointegração e é INFORMADO aqui. Quem o
escolhe é o `series/johansen`, pelo posto sugerido. Os coeficientes de ajuste
e o vetor de cointegração saem do `urca::cajorls`; a previsão e os impulsos
saem do `vars::vec2var`.

O resultado é um ajuste de `series/var`, com o tipo VECM: o card mostra os
coeficientes de cada equação e a previsão funciona como nos VARs.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Parâmetros

- **Posto (r)** — número de relações de cointegração, de 1 a k - 1.
- **Defasagens (K)** — defasagens do VAR em nível, no mínimo 2.
- **Determinístico** — constante restrita (só na relação de cointegração),
  constante livre (no VECM: séries com tendência) ou tendência restrita
  (tendência na relação, constante livre).
- **Dummies sazonais** — centradas por período, para série com ciclo.

## Valor

Um ajuste multivariado (`series/var`, tipo VECM).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("v", "series/vecm", posto = 1L, defasagens = 2L, from = "j")
```

## Veja também

`series/johansen`, para escolher o posto; `series/forecast`, para prever.

