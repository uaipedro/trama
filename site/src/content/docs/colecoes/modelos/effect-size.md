---
title: Tamanho de efeito (ANOVA)
description: "Eta², eta² parcial e ômega² de cada termo da ANOVA: quanto da variação cada um explica."
section: colecoes
collection: modelos
node: models/effect_size
category: resumir
related: [models/anova_table, models/cohen_d]
---

## O que o bloco faz

`models/effect_size` calcula, para cada termo do quadro da ANOVA, o eta² (fração da variação total), o eta² parcial (o termo contra o próprio erro) e o ômega² (o eta² corrigido do viés em amostra pequena). A saída é uma tabela.

## Quando usar

Sempre que o artigo pedir, além do p-valor, o tamanho do efeito de cada fator — em revistas que seguem as normas da APA isso é obrigatório. Funciona em `lm`, nos delineamentos e na parcela subdividida; GLM e misto não têm somas de quadrados e são recusados.

## Configuração

Soma de quadrados escolhe o tipo (`I`, `II` ou `III`), como no [Quadro da ANOVA](/trama/colecoes/modelos/anova-table/). Cada termo usa o próprio erro: na parcela subdividida, o erro (a) para o fator da parcela e o (b) para a subparcela e a interação; a coluna `erro` diz qual.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "milho_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "hibrido", bloco = "bloco", from = "dados") |>
  tr_add("efeito", "models/effect_size", from = "ajuste")
```

No DBC de milho, o híbrido explica cerca de 65% da variação total (eta² 0,65; eta² parcial 0,82; ômega² 0,60).

## Como interpretar

O eta² diminui quando o modelo ganha termos; o eta² parcial não, e por isso é o que se compara entre experimentos com delineamentos diferentes. O ômega² é o menos viesado e pode sair negativo quando F < 1: leia como zero. As referências de Cohen (0,01, 0,06 e 0,14 para eta²) são genéricas; efeitos já publicados na área são uma régua melhor.
