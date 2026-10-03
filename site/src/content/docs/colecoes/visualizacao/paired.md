---
title: Pareamento
description: Liga medidas repetidas da mesma unidade entre condições.
section: colecoes
collection: visualizacao
node: view/paired
category: comparacao
related: [view/dumbbell, view/strip, data/group_summarise]
---

## O que o bloco faz

`view/paired` liga medidas repetidas da mesma unidade entre condições.

## Quando usar

Use quando as mesmas unidades são medidas em dois ou mais tempos ou condições.

## Configuração

Condição, Medida numérica e Unidade são obrigatórias. Unidade × Condição não pode repetir; resuma repetições antes. Cor por agrupa unidades; Linha da média vem ligada. A ordem das condições segue níveis do fator ou ordem alfabética.

## Exemplo

```r
library(trama.view)
dados <- data.frame(fase = rep(c("antes", "depois"), 4),
                    altura = c(10, 12, 8, 9, 11, 13, 9, 11),
                    planta = rep(paste0("P", 1:4), each = 2))
tr_paired(dados, x = "fase", y = "altura", unidade = "planta", media = TRUE)
```

## Como interpretar

Cada segmento conecta medidas da mesma unidade, e sua inclinação mostra se aquela unidade aumentou ou diminuiu entre condições. A linha destacada, quando ligada, é a média de cada condição. A distribuição de inclinações revela consistência de mudança que médias isoladas podem ocultar.
