---
title: Matriz de confusão
description: "Classe real × classe prevista, com o acerto de cada classe e o geral."
section: colecoes
collection: modelos
node: models/confusion
category: avaliar
related: [models/roc, models/evaluate, models/predict]
---

## O que o bloco faz

`models/confusion` conta, para cada classe real, em qual classe o modelo pôs cada caso. A saída é `data/table` no formato largo: `real`, uma coluna por classe prevista, `total`, `acertos`, `taxa_acerto` e a linha `total` com o acerto geral.

## Quando usar

Use depois de ajustar um classificador — o GLM binomial, a discriminante ou a logística da coleção multivariada, as árvores de machine learning — para ver quais classes ele confunde.

## Configuração

As duas entradas são opcionais, mas uma tem de estar ligada. Só `modelo`: avalia no treino pela Validação (`cruzada`, padrão, ou `resubstituição`). `modelo` e `dados`: prevê a tabela e compara com a resposta do modelo. Só `dados`: modo tabela, com Resposta e Previsto (padrão `previsto`) nomeando as colunas.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("logit", "models/glm", formula = "am ~ wt", familia = "binomial", from = "dados") |>
  tr_add("cv", "models/confusion", validacao = "cruzada", from = "logit")
```

Cada carro é classificado como manual ou automático por um modelo ajustado sem ele.

## Como interpretar

A diagonal são os acertos. Leia a taxa de cada classe, não só a geral: com classes desbalanceadas, prever sempre a maior já acerta muito. A resubstituição é otimista; a cruzada estima o acerto num caso novo.
