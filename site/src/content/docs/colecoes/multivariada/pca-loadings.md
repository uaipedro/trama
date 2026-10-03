---
title: Cargas da PCA
description: Apresenta relação de cada variável com componentes como correlações ou autovetores.
section: colecoes
collection: multivariada
node: multi/pca_loadings
related: [multi/pca_variance, multi/biplot, multi/correlation_circle]
---

## O que o bloco faz

O bloco `multi/pca_loadings` extrai uma tabela com as variáveis nas linhas e componentes nas colunas, como correlações variável-componente ou autovetores. O bloco recebe `multi/pca`.

## Quando usar

Use **Cargas da PCA** para interpretar quais variáveis definem cada componente e com que direção.

## Configuração

- **Tipo** — `correlações` (padrão) ou `autovetores`.
- **Componentes** — número inicial de componentes; 0 (padrão) mostra todos.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", cols = "Murder, Assault, UrbanPop, Rape", from = "dados") |>
  tr_add("cargas", "multi/pca_loadings", tipo = "correlações", from = "pca")
```

A tabela devolve as correlações de cada variável com os componentes da PCA.

## Como interpretar

Correlação variável-componente fica entre −1 e 1. Autovetores são pesos para calcular escores; suas colunas têm soma de quadrados 1. Não compare os dois tipos como se estivessem na mesma escala.

## Veja também

- [`Variância explicada`](/trama/colecoes/multivariada/pca-variance/)
- [`Biplot`](/trama/colecoes/multivariada/biplot/)
- [`Círculo de correlações`](/trama/colecoes/multivariada/correlation-circle/)
