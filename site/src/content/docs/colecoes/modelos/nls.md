---
title: Regressão não linear
description: "Ajusta uma curva não linear pronta (logística, Michaelis-Menten, Gompertz, platô...) sem pedir chute."
section: colecoes
collection: modelos
node: models/nls
category: ajustar
related: [models/plot_regression, models/coefficients, models/dose_response]
---

## O que o bloco faz

`models/nls` ajusta por mínimos quadrados não lineares uma das curvas prontas — logístico, Michaelis-Menten, exponencial assintótico, Gompertz ou linear-platô — com a resposta e uma preditora numérica. Os valores iniciais saem dos próprios dados. A saída é `models/fit`.

## Quando usar

Use para curvas de crescimento (logístico, Gompertz), de absorção e saturação (Michaelis-Menten), de resposta a adubo que se aproxima de um teto (exponencial assintótico) ou de dose a partir da qual não há ganho (linear-platô).

## Configuração

Resposta e Preditor são colunas numéricas; Modelo escolhe a curva. Os quatro primeiros modelos usam os self-starters do R; o linear-platô procura o ponto de quebra de menor soma de quadrados. Uma curva por grupo não é feita aqui: filtre a tabela e ajuste cada grupo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "Puromycin") |>
  tr_add("resultado", "models/nls", resposta = "rate", preditor = "conc", modelo = "Michaelis-Menten", from = "dados")
```

A velocidade de reação satura com a concentração do substrato; o ajuste dá a velocidade máxima `Vm` e a concentração `K` em que se chega à metade dela.

## Como interpretar

Cada parâmetro tem significado na curva: assíntota, ponto de inflexão, início do platô. A tabela traz cada um com erro padrão e intervalo de Wald. O R² das medidas sai na coluna `r2_pseudo`, e o gráfico o rotula "R² (pseudo)"; para escolher entre curvas na mesma resposta, compare o AIC e o `rmse`. Quando o ajuste não converge, o card diz que forma o modelo espera.
