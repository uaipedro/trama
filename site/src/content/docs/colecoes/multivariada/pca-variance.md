---
title: Variância explicada
description: Devolve autovalores, proporção e proporção acumulada da variância de cada componente.
section: colecoes
collection: multivariada
node: multi/pca_variance
related: [multi/pca, multi/scree, multi/pca_loadings]
---

## O que o bloco faz

O bloco `multi/pca_variance` extrai do modelo PCA uma tabela com um registro por componente e as colunas `autovalor`, `desvio`, `proporcao` e `acumulada`. O bloco recebe `multi/pca`.

## Quando usar

Use **Variância explicada** para decidir quantos componentes analisar e quantos manter na redução de dimensão.

## Configuração

Sem parâmetros próprios; conecte um modelo `multi/pca`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", cols = "Murder, Assault, UrbanPop, Rape", from = "dados") |>
  tr_add("var", "multi/pca_variance", from = "pca")
```

A tabela devolve uma linha por componente CP e suas proporções explicadas.

## Como interpretar

`autovalor` é variância; `desvio` é sua raiz; `proporcao` soma 1 e `acumulada` soma as proporções sucessivas. Na PCA não padronizada, autovalores ficam na unidade ao quadrado das variáveis.

## Veja também

- [`Componentes principais`](/trama/colecoes/multivariada/pca/)
- [`Scree`](/trama/colecoes/multivariada/scree/)
- [`Cargas da PCA`](/trama/colecoes/multivariada/pca-loadings/)
