---
title: MANOVA
description: Análise de variância multivariada com Pillai, Wilks, Hotelling-Lawley ou Roy.
section: colecoes
collection: multivariada
node: multi/manova
related: [multi/box_m, multi/discriminant, models/anova_dbc]
---

**Use quando** você mediu várias respostas ao mesmo tempo e quer saber se os tratamentos diferem considerando todas juntas.

## O que o bloco faz

O bloco `multi/manova` testa se os tratamentos diferem no vetor de médias de várias respostas ao mesmo tempo (`stats::manova`), com bloco opcional (DBC). A saída é um teste (`data/test`).

## Configuração

- **Respostas** — duas ou mais colunas numéricas.
- **Tratamento** — coluna do tratamento.
- **Bloco** — em branco, inteiramente casualizado.
- **Estatística** — `Pillai` (padrão, a mais robusta), `Wilks`, `Hotelling-Lawley` ou `Roy`.

## Exemplo

```r
tr_flow(reg) |>
  tr_add("v", "multi/example", dataset = "vinhos") |>
  tr_add("man", "multi/manova", respostas = "alcool, flavonoides, magnesio",
         tratamento = "cultivar", estatistica = "Wilks", from = "v")
```

## Como interpretar

P-valor pequeno indica que os tratamentos diferem em alguma combinação das respostas; siga com ANOVAs por variável ou com a discriminante para saber onde.
