---
title: "Defasagens do VAR"
description: "AIC, HQ, SC e FPE para cada defasagem do VAR, com a que cada critério escolhe."
section: colecoes
collection: series-temporais
node: series/var_select
category: "Multivariada"
related: [series/var, series/join, series/portmanteau_mv]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Ajusta o VAR de cada ordem de 1 até **Máximo de defasagens** e mostra, por
ordem, os quatro critérios de informação do VAR: AIC, HQ, SC (BIC) e FPE.
Quanto menor, melhor. A coluna **escolhida** diz quais critérios apontam para
aquela ordem.

Os critérios penalizam o número de parâmetros de jeitos diferentes. AIC e FPE
penalizam pouco e tendem a escolher ordens maiores; SC (BIC) penaliza mais e
tende às menores, e é consistente quando a verdadeira ordem existe. Quando
discordam, é comum preferir a ordem em que os resíduos do `series/var` ficam
sem autocorrelação (`series/portmanteau_mv`) e que não deixa o VAR
instável.

A série não aceita faltantes: com buraco, o `series/interpolate` vem antes, ou
recorte a série com `series/window`.

A tabela usa as mesmas regras do `series/var`: o mesmo determinístico e as
mesmas dummies sazonais. A série precisa ser estacionária (ou ter sido
diferenciada) para a escolha fazer sentido.

## Parâmetros

- **Máximo de defasagens** — a última ordem testada.
- **Determinístico** — constante, tendência, ambos ou nenhum, em cada equação.
- **Dummies sazonais** — dummies centradas por período (série com ciclo).

## Valor

Uma tabela (`data/table`) com uma linha por defasagem: `defasagem`, `AIC`,
`HQ`, `SC`, `FPE` e `escolhida` (os critérios que escolhem aquela ordem).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("sel", "series/var_select", max_defasagens = 6L, from = "j")
```

## Veja também

`series/var`, que ajusta o modelo na ordem escolhida (com `defasagens = 0`, a
mesma conta); `series/join` para montar a série múltipla;
`series/portmanteau_mv` para os resíduos.

