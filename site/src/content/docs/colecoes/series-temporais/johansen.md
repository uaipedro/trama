---
title: "Johansen"
description: "Johansen: quantas relações de cointegração há, pelo traço ou pelo autovalor máximo."
section: colecoes
collection: series-temporais
node: series/johansen
category: "Testes multivariados"
related: [series/engle_granger, series/vecm]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Conta quantas relações de cointegração há entre as séries, pelo procedimento de
Johansen (1991), com o VAR em nível de K defasagens.

Para cada posto r0 = 0, 1, ... até k - 1, a tabela traz a estatística, os
críticos a 10, 5 e 1% e a decisão a 5%. O posto sugerido é o primeiro r0 em que
a hipótese não é rejeitada.

### Traço e autovalor

O **traço** testa `r <= r0` contra `r > r0`; o **autovalor máximo** testa
`r = r0` contra `r = r0 + 1`. Quando discordam, o traço é o mais citado e é o
padrão aqui.

### Determinísticos

Três casos, pelo que entra em cada parte do modelo:

- **constante restrita** (padrão) — a constante só dentro da relação de
  cointegração: as séries não têm tendência, e o equilíbrio tem nível.
- **constante livre** — constante solta no VECM: as séries têm tendência
  linear, a relação de cointegração não.
- **tendência restrita** — tendência dentro da relação e constante livre: a
  própria relação de equilíbrio tem tendência.

Os críticos mudam com o caso. Dummies sazonais entram centradas, e pedem série
com ciclo.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Parâmetros

- **Estatística** — traço (padrão) ou autovalor máximo.
- **Defasagens (K)** — defasagens do VAR em nível, no mínimo 2 (o VECM tem K - 1
  em diferença).
- **Determinístico** — constante restrita, constante livre ou tendência
  restrita (ver acima).
- **Dummies sazonais** — centradas por período, para série com ciclo.

## Valor

Uma tabela (`data/table`) com uma linha por posto: `hipotese`, `estatistica`,
`critico_10`, `critico_5`, `critico_1`, `decisao_5` e `nota`, que marca o posto
sugerido.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("jo", "series/johansen", metodo = "traco", defasagens = 2L, from = "j")
```

## Veja também

`series/engle_granger` para uma resposta só; `series/vecm`, com o posto
escolhido aqui.

