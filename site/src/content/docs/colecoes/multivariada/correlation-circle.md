---
title: Círculo de correlações
description: Mostra correlações das variáveis com dois componentes dentro do círculo unitário.
section: colecoes
collection: multivariada
node: multi/correlation_circle
related: [multi/pca_loadings, multi/biplot, multi/pca]
---

## O que o bloco faz

O bloco `multi/correlation_circle` plota, no círculo unitário, a correlação de cada variável com dois componentes de uma PCA. O bloco recebe `multi/pca`.

## Quando usar

Use **Círculo de correlações** para verificar quais variáveis são bem representadas em um plano e interpretar suas relações com os eixos.

## Configuração

- **Componente X** e
- **Componente Y** — índices de eixos diferentes (1 e 2 por padrão). **Aspecto**, **Tema**, **Título**, **Rótulo X**, **Rótulo Y** e **Legenda** controlam a apresentação.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", cols = "Murder, Assault, UrbanPop, Rape", from = "dados") |>
  tr_add("circ", "multi/correlation_circle", x = 1L, y = 2L, from = "pca")
```

O gráfico mostra correlações de `Murder`, `Assault`, `UrbanPop` e `Rape` com CP1 e CP2.

## Como interpretar

As coordenadas das setas são correlações exatas. `cos²` é a soma dos quadrados das duas coordenadas; valor próximo de 1 indica boa representação no plano. Ângulos de setas curtas não são informativos.

## Veja também

- [`Cargas da PCA`](/trama/colecoes/multivariada/pca-loadings/)
- [`Biplot`](/trama/colecoes/multivariada/biplot/)
- [`Componentes principais`](/trama/colecoes/multivariada/pca/)
