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

Informe resposta, preditores ou Fórmula (que prevalece) e Família: binomial para resposta binária ou sucessos em n tentativas (`cbind(sucessos, fracassos) ~ ...` na fórmula), Poisson para contagem, gama para contínua positiva, gaussiana para contínua. Com superdispersão, `quasipoisson` (contagem) e `quasibinomial` (binomial agregada) estimam a dispersão pelo X² de Pearson dividido pelos gl do resíduo; os coeficientes não mudam, os erros padrão crescem pela raiz da dispersão e o quadro passa a F.

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

Com a binomial agregada, compare o desvio residual com os gl do resíduo. No `cbpp` (lme4), a incidência de pleuropneumonia bovina por período, `cbind(incidence, size - incidence) ~ period` com `quasibinomial` dá dispersão 2,19: os erros padrão da binomial ficam multiplicados por 1,48, e o F do período é 6,20 (p = 0,0011).
