---
title: Mapa de correlações
description: Mostra a matriz de correlação como mapa de calor, com ordenação por agrupamento e valores opcionais.
section: colecoes
collection: multivariada
node: multi/plot_correlation
related: [multi/correlation_matrix, multi/pca]
---

## O que o bloco faz

O bloco `multi/plot_correlation` calcula correlações de Pearson entre colunas e as exibe como mapa de calor. A saída é um gráfico. O bloco recebe `data/table`.

## Quando usar

Use **Mapa de correlações** para localizar blocos de variáveis associadas antes de escolher uma PCA ou análise fatorial.

## Configuração

**Variáveis** — colunas da matriz. **Ordenar por agrupamento** e **Mostrar valores** são opções booleanas, ligadas por padrão. **Aspecto**, **Tema**, **Título**, **Rótulo X**, **Rótulo Y** e **Legenda** controlam apresentação.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "questionario") |>
  tr_add("mapa", "multi/plot_correlation", cols = "ans1, ans2, ans3, soc1, soc2", from = "dados")
```

O mapa mostra correlações de Pearson entre os cinco itens, ordenados pelo padrão de associação.

## Como interpretar

Vermelho indica correlação positiva e azul, negativa; intensidade representa magnitude. A diagonal vale 1. O bloco usa Pearson; a ordenação por agrupamento aproxima variáveis com padrões semelhantes.

## Veja também

- [`Matriz de correlação`](/trama/colecoes/multivariada/correlation-matrix/)
- [`Componentes principais`](/trama/colecoes/multivariada/pca/)
