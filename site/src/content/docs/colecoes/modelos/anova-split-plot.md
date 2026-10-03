---
title: ANOVA · parcela subdividida
description: "Análise de variância de parcelas subdivididas em blocos, com os erros (a) e (b)."
section: colecoes
collection: modelos
node: models/anova_split_plot
category: anova
related: [models/anova_table, models/lmer]
---

## O que o bloco faz

`models/anova_split_plot` Ajusta parcelas subdivididas com erros separados por estrato. A saída é `models/fit`.

## Quando usar

Use quando um fator é sorteado em parcelas maiores e outro em subparcelas, com precisão diferente para cada fator.

## Configuração

Informe resposta, fator sorteado na Parcela, fator da Subparcela e Bloco; essa ordem determina os estratos.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "aveia") |>
  tr_add("ajuste", "models/anova_split_plot", resposta = "producao", parcela = "variedade", subparcela = "nitrogenio", bloco = "bloco", from = "dados")
```

`aveia` traz variedade na parcela e nitrogênio na subparcela, com bloco como restrição.

## Como interpretar

Use resíduo (a) para testar bloco e fator da parcela; resíduo (b) para subparcela e interação.
