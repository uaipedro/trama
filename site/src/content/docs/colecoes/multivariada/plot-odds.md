---
title: Gráfico das razões de chances
description: Plota razões de chances por preditor com intervalos em escala log.
section: colecoes
collection: multivariada
node: multi/plot_odds
related: [multi/logistic_coefficients, multi/logistic, multi/roc]
---

## O que o bloco faz

O bloco `multi/plot_odds` plota razões de chances e intervalos de confiança em escala logarítmica. A saída é um gráfico. O bloco recebe `multi/logit`.

## Quando usar

Use **Gráfico das razões de chances** para comparar direção e incerteza dos efeitos dos preditores no mesmo ajuste.

## Configuração

**Escala** — `desvio padrão` (padrão) ou `unidade`. Opções visuais: **Aspecto**, **Tema**, **Título**, **Rótulo X**, **Rótulo Y** e **Legenda**.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "pima") |>
  tr_add("log", "multi/logistic", resposta = "diabetes", preditores = "glicose, imc, pedigree", from = "dados") |>
  tr_add("od", "multi/plot_odds", from = "log")
```

O gráfico compara razões de chances padronizadas e intervalos entre preditores.

## Como interpretar

A linha de referência em 1 representa razão de chances nula. Intervalo que a cruza inclui ausência de efeito no nível configurado. Escala por desvio padrão facilita comparar preditores com unidades diferentes.

## Veja também

- [`Razões de chances`](/trama/colecoes/multivariada/logistic-coefficients/)
- [`Regressão logística`](/trama/colecoes/multivariada/logistic/)
- [`Curva ROC`](/trama/colecoes/multivariada/roc/)
