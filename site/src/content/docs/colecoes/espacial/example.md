---
title: "Exemplo espacial"
description: "Carrega um conjunto de exemplo do IBGE já como objeto espacial: coordenadas, projeção e borda do estado."
section: colecoes
collection: espacial
node: spatial/example
category: "Fonte"
related: [spatial/coordinates]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Carrega um conjunto real, do IBGE, pensado para ensinar geoestatística. Os três
vêm da mesma fonte: o rendimento médio da produção (kg/ha) da Produção Agrícola
Municipal de 2023 (SIDRA, tabela 5457), medido nas sedes municipais. As
coordenadas são UTM em **metros** (SIRGAS 2000), e a borda é a do estado.

- **milho_pr** — milho no Paraná, 389 municípios, com o rendimento da soja
  (`soja_kg_ha`) como covariável. Dependência espacial **forte** e tendência de
  larga escala: o variograma não estabiliza nos primeiros 300 km. É o caso que
  pede a remoção de tendência.
- **cafe_mg** — café em Minas Gerais, 496 municípios. Dependência espacial
  **moderada**, com alcance ajustado de várias centenas de quilômetros.
- **milho_se** — milho em Sergipe, 68 municípios. Dependência forte, alcance
  perto de 110 km. Conjunto pequeno, para exemplo rápido.

Fonte: IBGE. Os dados são abertos, e o uso exige citar a fonte.

## Parâmetros

- **Conjunto** — um dos três acima.

## Valor

Um objeto espacial (`spatial/points`), com coordenadas, projeção, unidade e borda.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("pontos", "spatial/example", dataset = "milho_pr")
```

## Veja também

`spatial/coordinates` para declarar o mesmo tipo de objeto a partir de uma tabela
sua.

