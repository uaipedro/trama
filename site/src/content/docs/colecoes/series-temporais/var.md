---
title: "VAR"
description: "Ajusta um vetor autorregressivo VAR(p): cada série explicada pelo passado de todas."
section: colecoes
collection: series-temporais
node: series/var
category: "Multivariada"
related: [series/var_select, series/granger, series/irf, series/portmanteau_mv]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Ajusta um VAR(p) por mínimos quadrados, equação a equação: cada série é
regredida nas `p` defasagens de TODAS as séries. É o modelo de partida para
séries que se influenciam — previsão conjunta, causalidade de Granger,
impulso-resposta.

As séries devem ser estacionárias. Em nível e cointegradas, use o
`series/vecm`; em nível e não cointegradas, diferencie antes. O card mostra os
coeficientes de cada equação e o resumo, a maior raiz do polinômio: acima de 1,
o VAR é instável. O bloco não aceita faltantes: interpole antes cada série
(`series/interpolate`) ou recorte o período.

## Parâmetros

- **Defasagens (p)** — 0 escolhe pelo critério, até o máximo.
- **Critério** — AIC (padrão), HQ, SC (BIC) ou FPE.
- **Determinístico** — constante, tendência, ambos ou nenhum, em cada equação.
- **Dummies sazonais** — dummies centradas por período (série com ciclo).

## Valor

Um ajuste multivariado (`series/var`).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", from = c("a", "b")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("prev", "series/forecast", horizonte = 8L, from = "v")
```

## Veja também

`series/var_select` para comparar os critérios; `series/granger`;
`series/irf`; `series/portmanteau_mv` para os resíduos.

