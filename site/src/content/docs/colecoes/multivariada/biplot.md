---
title: Biplot
description: Representa observações e variáveis no plano de dois componentes.
section: colecoes
collection: multivariada
node: multi/biplot
related: [multi/pca, multi/pca_loadings, multi/correlation_circle]
---

## O que o bloco faz

O bloco `multi/biplot` representa escores das observações e setas das variáveis no plano de dois componentes selecionados. A saída é um gráfico. O bloco recebe `multi/pca`.

## Quando usar

Use **Biplot** para observar simultaneamente a posição dos casos e as direções das variáveis na mesma projeção.

## Configuração

- **Componente X** e
- **Componente Y** — índices de dois componentes diferentes (1 e 2 por padrão). **Cor por** e
- **Rótulo** — colunas opcionais da tabela original.
- **Setas das variáveis** — ligadas por padrão. Opções visuais: **Aspecto**, **Tema**, **Título**, **Rótulo X**, **Rótulo Y** e **Legenda**.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", cols = "Murder, Assault, UrbanPop, Rape", from = "dados") |>
  tr_add("bp", "multi/biplot", x = 1L, y = 2L, from = "pca")
```

O gráfico combina escores dos estados e setas das quatro variáveis no plano CP1–CP2.

## Como interpretar

Pontos próximos têm escores semelhantes no plano. Setas paralelas indicam associação positiva e opostas, associação negativa. Os vetores são reescalados para caber nos escores; comprimento absoluto deve ser lido no círculo de correlações.

## Veja também

- [`Componentes principais`](/trama/colecoes/multivariada/pca/)
- [`Cargas da PCA`](/trama/colecoes/multivariada/pca-loadings/)
- [`Círculo de correlações`](/trama/colecoes/multivariada/correlation-circle/)
