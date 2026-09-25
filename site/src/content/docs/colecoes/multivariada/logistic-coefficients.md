---
title: Razões de chances
description: Apresenta coeficientes, erros padrão, p-valores e razões de chances com intervalo (Wald na ML, perfilado no Firth).
section: colecoes
collection: multivariada
node: multi/logistic_coefficients
related: [multi/logistic, multi/plot_odds, multi/roc]
---

## O que o bloco faz

O bloco `multi/logistic_coefficients` extrai coeficientes, erros padrão, testes, razões de chances e intervalos de confiança em uma tabela. Na logística por máxima verossimilhança o intervalo e o p são de Wald; na de Firth (`metodo = "firth"` na `multi/logistic`), são da verossimilhança penalizada perfilada (Heinze & Schemper, 2002), e a coluna `intervalo` diz qual. O bloco recebe `multi/logit`.

## Quando usar

Use **Razões de chances** para quantificar direção e tamanho dos efeitos estimados pelos preditores.

## Configuração

- **Escala** — `unidade` (padrão) ou `desvio padrão`, para expressar o efeito por desvio padrão da variável.
- **Nível do intervalo** — 0,95 por padrão, entre 0,5 e 0,999.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "pima") |>
  tr_add("log", "multi/logistic", grupo = "diabetes", cols = "glicose, imc, pedigree", from = "dados") |>
  tr_add("coef", "multi/logistic_coefficients", from = "log")
```

A tabela resume coeficientes e razões de chances da logística ajustada.

## Como interpretar

Coeficiente está em log-chances; razão de chances é `exp(coeficiente)`. Valor 1 indica ausência de mudança na chance por unidade. Intervalo que inclui 1 corresponde a coeficiente compatível com 0.

## Veja também

- [`Regressão logística`](/trama/colecoes/multivariada/logistic/)
- [`Gráfico das razões de chances`](/trama/colecoes/multivariada/plot-odds/)
- [`Curva ROC`](/trama/colecoes/multivariada/roc/)
