---
title: M de Box
description: Testa igualdade das matrizes de covariância entre grupos para apoiar a escolha LDA/QDA.
section: colecoes
collection: multivariada
node: multi/box_m
related: [multi/discriminant, models/confusion]
---

## O que o bloco faz

O bloco `multi/box_m` calcula o teste M de Box para igualdade das matrizes de covariância dos grupos. O bloco recebe `data/table`.

## Quando usar

Use **M de Box** como diagnóstico do pressuposto de covariância comum da LDA e como parte da decisão entre LDA e QDA.

## Configuração

- **Grupo** — coluna categórica com as classes.
- **Variáveis** — medidas numéricas; em branco, usa as numéricas menos Grupo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "vinhos") |>
  tr_add("bx", "multi/box_m", grupo = "cultivar", cols = "alcool, flavonoides", from = "dados")
```

A tabela informa a estatística M de Box e seu p-valor para as medidas escolhidas.

## Como interpretar

P-valor pequeno indica evidência contra covariâncias iguais. O teste é sensível a desvios de normalidade; considere também desempenho validado e tamanho dos grupos.

## Veja também

- [`Discriminante`](/trama/colecoes/multivariada/discriminant/)
- [`Matriz de confusão`](/trama/colecoes/modelos/confusion/)
