---
title: Friedman
description: "Friedman: os tratamentos diferem dentro dos blocos? (DBC não paramétrico)"
section: colecoes
collection: modelos
node: models/friedman
category: testes
related: [models/anova_dbc, models/kruskal]
---

## O que o bloco faz

`models/friedman` ordena os tratamentos dentro de cada bloco e compara as somas de postos (Friedman 1937). É a alternativa por postos ao `models/anova_dbc`. A saída é `models/test`, com o W de Kendall como efeito.

## Quando usar

Num DBC de um fator em que os resíduos não são normais e nenhuma transformação resolve. Pede uma observação por bloco e tratamento: com repetições, resuma antes pela média de cada casela. Bloco a que falta tratamento sai inteiro, e a nota conta quantos.

## Configuração

Informe a resposta numérica, a coluna do tratamento e a do bloco.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("fr", "models/friedman", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho")
```

Os 5 híbridos do `milho_dbc` em 4 blocos dão qui-quadrado de Friedman 13,6 com 4 gl, p = 0,0087, e W de Kendall 0,85: a ordem dos híbridos se repete quase igual em todos os blocos.

## Como interpretar

A hipótese nula é que, dentro de cada bloco, todos os tratamentos têm a mesma distribuição. Rejeitar diz que algum tratamento difere, não qual. Empates dentro do bloco recebem posto médio e a estatística é corrigida. O p-valor vem da aproximação qui-quadrado, que fica grosseira com poucos blocos (como os 4 do exemplo).
