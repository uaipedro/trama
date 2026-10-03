---
title: Componentes principais
description: Reduz variáveis correlacionadas a componentes ortogonais que resumem a variância da tabela.
section: colecoes
collection: multivariada
node: multi/pca
related: [multi/pca_variance, multi/pca_loadings, multi/biplot]
---

## O que o bloco faz

O bloco `multi/pca` centraliza as variáveis e ajusta `stats::prcomp`, retornando um modelo com escores, autovetores e variância dos componentes. O bloco recebe `data/table`.

## Quando usar

Use **Componentes principais** para representar variáveis correlacionadas por um conjunto menor de componentes ortogonais e para explorar padrões multivariados.

## Configuração

- **Variáveis** — colunas numéricas; em branco, todas.
- **Padronizar** — ligado por padrão, calcula PCA da matriz de correlação; desligado, usa a matriz de covariância. A centralização permanece ligada nas duas opções.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", cols = "Murder, Assault, UrbanPop, Rape", padronizar = TRUE, from = "dados")
```

O ajuste PCA alimenta os nós de variância, cargas e gráficos que seguem.

## Como interpretar

Cada componente maximiza a variância restante. Com Padronizar ligado, a PCA usa a matriz de correlação; desligado, usa covariância. Em `USArrests`, padronizar impede que a escala maior de `Assault` domine o primeiro componente.

## Veja também

- [`Variância explicada`](/trama/colecoes/multivariada/pca-variance/)
- [`Cargas da PCA`](/trama/colecoes/multivariada/pca-loadings/)
- [`Biplot`](/trama/colecoes/multivariada/biplot/)
