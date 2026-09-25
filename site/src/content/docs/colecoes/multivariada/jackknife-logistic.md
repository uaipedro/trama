---
title: Jackknife da logística
description: Estima incerteza e influência nos coeficientes ou razões de chances ao reajustar sem cada linha.
section: colecoes
collection: multivariada
node: multi/jackknife_logistic
related: [multi/logistic, multi/logistic_coefficients, multi/roc]
---

## O que o bloco faz

O bloco `multi/jackknife_logistic` refaz a logística sem cada observação e estima incerteza e influência em coeficientes ou razões de chances. O bloco recebe `multi/logit`.

## Quando usar

Use **Jackknife da logística** para localizar linhas influentes e comparar erro padrão jackknife com a aproximação de Wald.

## Configuração

- **Estatística** — `coeficientes` (padrão) ou `razões de chances`.
- **Tabela** — `resumo` (padrão) ou `pseudovalores`.
- **Nível do intervalo** — 0,95 por padrão, entre 0,5 e 0,999.
- **Grupo (apagar-um-grupo)** — em branco (padrão), tira uma linha por vez. Com dados em conglomerados (várias linhas do mesmo talhão, animal ou lote), informe a coluna do conglomerado: cada réplica tira o grupo inteiro e o erro padrão usa o número de grupos G no lugar de n, com intervalo t(G − 1) — a variância JK1 de amostragem (Shao & Tu, 1995; Kott, 2001). Os pseudovalores saem um por grupo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "pima") |>
  tr_add("amostra", "data/slice_head", n = 150L, from = "dados") |>
  tr_add("log", "multi/logistic", grupo = "diabetes", cols = "glicose, imc", from = "amostra") |>
  tr_add("jk", "multi/jackknife_logistic", estatistica = "razões de chances", from = "log")
```

O fluxo usa as primeiras 150 linhas, como no teste da coleção; o resumo permite comparar erro padrão jackknife e de Wald.

## Como interpretar

Para razões de chances, estimativas e intervalo são exponenciados; viés, erro padrão e pseudovalores permanecem na escala log. Se uma réplica apresentar separação, a linha é indicada no erro.

## Veja também

- [`Regressão logística`](/trama/colecoes/multivariada/logistic/)
- [`Razões de chances`](/trama/colecoes/multivariada/logistic-coefficients/)
- [`Curva ROC`](/trama/colecoes/multivariada/roc/)
