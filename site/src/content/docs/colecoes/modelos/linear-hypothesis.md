---
title: Contrastes (F)
description: "Teste F da hipótese linear geral: você escreve os contrastes, nas médias de um fator ou nos coeficientes."
section: colecoes
collection: modelos
node: models/linear_hypothesis
category: medias
related: [models/duncan, models/emmeans]
---

## O que o bloco faz

`models/linear_hypothesis` Avalia contrastes lineares conjuntos por teste F. A saída é `models/test`.

## Quando usar

Use quando a hipótese científica combina vários coeficientes ou médias em um contraste conjunto.

## Configuração

Informe uma hipótese por linha; Fator seleciona médias do fator, ou deixe vazio para contrastes de coeficientes.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados") |>
  tr_add("resultado", "models/linear_hypothesis", hipoteses = "grouptrt1 = 0", from = "ajuste")
```

O contraste testa se o coeficiente do nível `trt1` difere de zero em relação ao nível de referência.

## Como interpretar

O resultado testa a combinação linear especificada, não cada coeficiente separadamente.
