---
title: Funções discriminantes
description: Resume autovalores, separação, correlação canônica e lambda de Wilks ou coeficientes das funções.
section: colecoes
collection: multivariada
node: multi/discriminant_functions
related: [multi/discriminant, multi/plot_discriminant, multi/confusion]
---

## O que o bloco faz

O bloco `multi/discriminant_functions` calcula estatísticas das funções discriminantes ou devolve seus coeficientes como tabela. O bloco recebe `multi/lda`.

## Quando usar

Use **Funções discriminantes** para avaliar a separação explicada por cada função ou interpretar as combinações lineares que formam os eixos LD.

## Configuração

**Tabela** — `funções` (padrão), `coeficientes`, `padronizados` ou `estrutura`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", grupo = "Species", from = "dados") |>
  tr_add("fun", "multi/discriminant_functions", from = "lda")
```

A tabela apresenta funções e estatísticas da separação por espécie.

## Como interpretar

Autovalores e proporção indicam contribuição de cada função; correlação canônica relaciona escores e grupos; lambda de Wilks resume a separação remanescente. Coeficientes são pesos conjuntos, não medidas isoladas de importância.

## Veja também

- [`Discriminante`](/trama/colecoes/multivariada/discriminant/)
- [`Plano discriminante`](/trama/colecoes/multivariada/plot-discriminant/)
- [`Matriz de confusão`](/trama/colecoes/multivariada/confusion/)
