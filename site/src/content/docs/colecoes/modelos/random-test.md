---
title: Teste dos aleatórios
description: "Razão de verossimilhança para cada termo aleatório de um modelo misto."
section: colecoes
collection: modelos
node: models/random_test
category: resumir
related: [models/anova_table, models/coefficients]
---

## O que o bloco faz

`models/random_test` Testa termos aleatórios por razão de verossimilhanças. A saída é `models/effects`.

## Quando usar

Use para avaliar se cada componente aleatório contribui para a estrutura do modelo misto.

## Configuração

Não há parâmetros adicionais; requer ajuste misto.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "sleepstudy") |>
  tr_add("ajuste", "models/lmer", formula = "Reaction ~ Days + (Days | Subject)", from = "dados") |>
  tr_add("resultado", "models/random_test", from = "ajuste")
```

Para o ajuste de `sleepstudy`, cada linha testa a contribuição de um termo aleatório por razão de verossimilhanças em relação ao modelo reduzido.

## Como interpretar

A comparação testa cada termo contra sua remoção. Variâncias na fronteira zero tornam a referência assintótica aproximada.
