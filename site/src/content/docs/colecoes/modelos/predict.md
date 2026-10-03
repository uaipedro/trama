---
title: Prever
description: "Aplica um modelo já ajustado a uma tabela nova (ou ao próprio treino), e devolve a previsão de cada linha."
section: colecoes
collection: modelos
node: models/predict
category: avaliar
related: [models/lm, models/glm, models/confusion, models/evaluate]
---

## O que o bloco faz

`models/predict` anexa a previsão de cada observação a dados novos — ou, sem `dados` ligada, às linhas do próprio ajuste. A saída é `data/table` com `previsto`, `prob_<nível>` na classificação e, quando pedidos, limites.

## Quando usar

Use após ajustar o modelo para aplicar a equação a linhas novas que contenham os preditores necessários.

## Configuração

Sem `dados`, Validação escolhe `resubstituição` (padrão) ou `cruzada` (cada linha prevista por um modelo ajustado sem ela). Intervalo pode ser nenhum, confiança ou predição, no nível de Confiança (padrão 0,95); os dois intervalos estão disponíveis para `lm`. Dados novos precisam conter preditores e níveis fatoriais vistos no ajuste.

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

Previsão está na escala da resposta. Num GLM binomial de resposta 0/1, `previsto` é a classe (corte 0,5) e a probabilidade está em `prob_1` — até a Fase 4 ela saía em `previsto`. Confiança cobre a média esperada; predição também inclui a variação de uma nova observação.
