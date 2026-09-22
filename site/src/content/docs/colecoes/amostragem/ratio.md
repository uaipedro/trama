---
title: Razão
description: Estima a razão entre dois totais (produtividade, renda per capita), com o erro do desenho.
section: colecoes
collection: amostragem
node: sampling/ratio
related: [sampling/mean, sampling/total, sampling/plot_estimates]
---

## O que o bloco faz

A razão entre dois totais, R = Σ w·y / Σ w·x: toneladas por hectare, renda por
morador, trabalhadores por fazenda. Não é a média das razões de cada unidade
(que daria à fazenda de 1 ha o mesmo peso que à de 1.000 ha): é a produção
total sobre a área total.

O denominador também é aleatório — outra amostra traria outra área —, e a
variância leva isso em conta pela linearização. A média de uma variável é o
caso particular com denominador 1 em toda unidade.

O deff não é calculado para a razão: a AAS de comparação dependeria da
variância conjunta das duas colunas, e um número a mais que se lê errado é pior
que nenhum.

## Quando usar

Use para estimar uma razão entre dois totais, como produção total por área total.

## Configuração

- **Numerador**, **Denominador** — colunas numéricas.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop") |>
  tr_add("razao", "sampling/ratio", numerador = "producao_t", denominador = "area_ha",
         por = "regiao", from = "amostra")
```

## Como interpretar

A razão divide o total ponderado do numerador pelo total ponderado do denominador; não equivale à média das razões individuais. A variância inclui a incerteza dos dois totais pela linearização. A saída é Estimativa `sampling/estimate` da razão.

## Veja também

`sampling/mean`, `sampling/total`, `sampling/plot_estimates`.
