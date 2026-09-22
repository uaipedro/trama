---
title: Modelo misto
description: "Ajusta um modelo linear misto (lme4), com p-valores por Satterthwaite (lmerTest)."
section: colecoes
collection: modelos
node: models/lmer
category: ajustar
related: [models/random_effects, models/random_test]
---

## O que o bloco faz

`models/lmer` Ajusta um modelo misto com `lme4` e produz `models/fit`; os p-valores dos efeitos fixos usam aproximação de Satterthwaite. A saída é `models/fit`.

## Quando usar

Use quando as observações se agrupam por pessoa, bloco, local ou outra unidade amostrada e essa variação deve entrar como efeito aleatório.

## Configuração

Use Fórmula com termos como `(1 | grupo)` ou `(tempo | pessoa)`. Sem fórmula, informe Resposta, Efeitos fixos e Grupo. REML controla o método de estimação das variâncias.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "sleepstudy") |>
  tr_add("ajuste", "models/lmer", formula = "Reaction ~ Days + (Days | Subject)", from = "dados")
```

`sleepstudy` contém medidas repetidas de reação por pessoa ao longo dos dias.

## Como interpretar

R² marginal resume efeitos fixos; condicional inclui os aleatórios. Variâncias descrevem heterogeneidade entre grupos.
