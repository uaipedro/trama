---
title: Waller-Duncan
description: "Teste de Waller-Duncan (bayesiano, razão K): letras de agrupamento das médias."
section: colecoes
collection: modelos
node: models/waller_duncan
category: medias
related: [models/duncan, models/emmeans]
---

## O que o bloco faz

`models/waller_duncan` Aplica Waller–Duncan às médias e retorna grupos por letras. A saída é `models/emm`.

## Quando usar

Use quando a análise especifica Waller–Duncan e uma razão K entre custos dos erros.

## Configuração

Tratamento escolhe o fator; K pondera custos de erros tipo I e II.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("resultado", "models/waller_duncan", tratamento = "hibrido", from = "ajuste")
```

Para o ajuste em parcelas subdivididas, o teste agrupa as médias de `nitrogenio`; a saída inclui estimativas e letras segundo a razão K configurada.

## Como interpretar

Letras agrupam médias segundo o procedimento bayesiano e a razão K escolhida.
