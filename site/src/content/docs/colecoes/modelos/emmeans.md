---
title: Médias ajustadas
description: "Médias ajustadas (emmeans) com intervalo de confiança e letras de comparação (Tukey e outros)."
section: colecoes
collection: modelos
node: models/emmeans
category: medias
related: [models/pairwise, models/plot_means]
---

## O que o bloco faz

`models/emmeans` Calcula médias marginais ajustadas com intervalos e letras de comparação. A saída é `models/emm`.

## Quando usar

Use para comparar níveis de fatores após controlar os outros termos do modelo, inclusive em desenho desbalanceado.

## Configuração

Médias de seleciona o fator; Por desdobra por outros fatores; ajuste das letras e Confiança (padrão 0,95; letras a alfa = 1 − confiança) definem comparações; Escala escolhe resposta ou ligação em GLM.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("resultado", "models/emmeans", especs = "hibrido", from = "ajuste")
```

O fluxo usa o delineamento de milho e calcula as médias de cada híbrido.

## Como interpretar

Médias ajustadas controlam os demais termos do modelo. Letras compartilhadas indicam que o procedimento não separou aquelas médias no nível de confiança escolhido.
