---
title: Modelo linear generalizado
description: "Ajusta um GLM (binomial, Poisson, gama...) pelas colunas ou por uma fórmula."
section: colecoes
collection: modelos
node: models/glm
category: ajustar
related: [models/coefficients, models/emmeans]
---

## O que o bloco faz

`models/glm` Ajusta um modelo linear generalizado e produz `models/fit`. A saída é `models/fit`.

## Quando usar

Use quando a resposta exigir distribuição diferente da normal, como contagem, resposta binária ou medida positiva assimétrica.

## Configuração

Informe resposta, preditores ou Fórmula (que prevalece) e Família: binomial para resposta binária, Poisson para contagem, gama para contínua positiva, gaussiana para contínua.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "InsectSprays") |>
  tr_add("ajuste", "models/glm", resposta = "count", preditores = "spray", familia = "poisson", from = "dados")
```

`InsectSprays` fornece contagens por inseticida para o ajuste Poisson.

## Como interpretar

Coeficientes estão na escala da ligação; previsões podem estar na escala da resposta. Poisson supõe variância igual à média; sob superdispersão, erros padrão podem ser otimistas.
