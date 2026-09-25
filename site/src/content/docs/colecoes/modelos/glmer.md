---
title: Misto generalizado
description: "Ajusta um modelo misto generalizado (lme4::glmer), binomial ou Poisson, com efeitos aleatórios."
section: colecoes
collection: modelos
node: models/glmer
category: ajustar
related: [models/lmer, models/glm, models/random_effects]
---

## O que o bloco faz

`models/glmer` ajusta um modelo misto generalizado com `lme4::glmer`: resposta binomial ou de contagem, como no `models/glm`, com efeitos aleatórios, como no `models/lmer`. A saída é `models/fit`.

## Quando usar

Use para proporções ou contagens com dados agrupados: plantas doentes por parcela com bloco aleatório, insetos por armadilha com local aleatório, germinação por placa com lote aleatório.

## Configuração

Fórmula recebe os efeitos fixos e os termos aleatórios na sintaxe do `lme4`, como `(1 | bloco)`; para uma proporção com o total da linha, a resposta vai como `cbind(sucessos, fracassos)`. Sem fórmula, Resposta, Efeitos fixos e Grupo montam um intercepto aleatório por grupo. Família escolhe `binomial` (logit) ou `poisson` (log).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "cbpp") |>
  tr_add("resultado", "models/glmer", formula = "cbind(incidence, size - incidence) ~ period + (1 | herd)", familia = "binomial", from = "dados")
```

A incidência de pleuropneumonia cai do primeiro para os outros períodos, com a variação entre rebanhos como efeito aleatório.

## Como interpretar

Os coeficientes estão na escala da ligação, com z de Wald; exponenciados em `models/coefficients`, são razões de chances (binomial) ou de taxas (Poisson). O quadro do `models/anova_table` sai por Wald, tipo II. Avisos de convergência ou de ajuste singular do `lme4` aparecem na nota dos coeficientes.
