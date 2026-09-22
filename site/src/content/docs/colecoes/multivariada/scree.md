---
title: Scree
description: Plota variância explicada por componente, proporção acumulada e referência de Kaiser.
section: colecoes
collection: multivariada
node: multi/scree
related: [multi/pca_variance, multi/pca]
---

## O que o bloco faz

O bloco `multi/scree` desenha a proporção explicada por componente como barras e a proporção acumulada como linha. Recebe um modelo PCA e produz um gráfico. O bloco recebe `multi/pca`.

## Quando usar

Use **Scree** para inspecionar o cotovelo da variância explicada e comparar com a recomendação da análise paralela.

## Configuração

Conecte um modelo `multi/pca`. **Aspecto**, **Tema**, **Título**, **Rótulo X**, **Rótulo Y** e **Legenda** controlam o gráfico.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", cols = "Murder, Assault, UrbanPop, Rape", from = "dados") |>
  tr_add("graf", "multi/scree", from = "pca")
```

O gráfico resultante mostra proporções individuais e acumuladas dos componentes.

## Como interpretar

O eixo está em proporção da variância. A linha horizontal de Kaiser (autovalor 1) só aparece para PCA padronizada; não se aplica à PCA por covariância.

## Veja também

- [`Variância explicada`](/trama/colecoes/multivariada/pca-variance/)
- [`Componentes principais`](/trama/colecoes/multivariada/pca/)
