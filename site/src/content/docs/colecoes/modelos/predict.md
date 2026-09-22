---
title: Prever
description: "Aplica um modelo já ajustado a uma tabela nova, e devolve a previsão de cada linha."
section: colecoes
collection: modelos
node: models/predict
category: resumir
related: [models/lm, models/glm]
---

## O que o bloco faz

`models/predict` Anexa a previsão de cada observação a dados novos. A saída é `data/table` com `previsto` e, quando pedidos, limites.

## Quando usar

Use após ajustar o modelo para aplicar a equação a linhas novas que contenham os preditores necessários.

## Configuração

Intervalo pode ser nenhum, confiança ou predição; os dois intervalos estão disponíveis para `lm`. Dados novos precisam conter preditores e níveis fatoriais vistos no ajuste.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("ajuste", "models/lm", formula = "mpg ~ wt", from = "dados") |>
  tr_add("prev", "models/predict", intervalo = "nenhum", from = c("ajuste", "dados"))
```

O ajuste e os dados novos compartilham `wt`, preditor usado para estimar `mpg`.

## Como interpretar

Previsão está na escala da resposta. Confiança cobre a média esperada; predição também inclui a variação de uma nova observação.
