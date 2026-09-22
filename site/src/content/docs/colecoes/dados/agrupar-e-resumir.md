---
title: Agrupar e resumir
description: Calcule estatísticas descritivas e outras medidas para a tabela inteira ou para cada grupo.
section: colecoes
collection: dados
node: data/group_summarise
category: agregar
order: 4
related: [data/summary, data/read_csv]
---

## O que o bloco faz

`data/group_summarise` calcula uma ou mais medidas sobre a tabela. Com grupos,
produz uma linha para cada combinação distinta das colunas escolhidas. Com
**Agrupar por** vazio, produz uma linha para a tabela inteira.

Cada medida ocupa uma coluna da saída, junto das colunas usadas para formar
os grupos. O resultado é uma tabela comum, pronta para visualização, junção ou
gravação.

## Quando usar

Use **Agrupar e resumir** quando a pergunta pede uma medida por categoria ou
para a tabela inteira. No exemplo, a média mensal de `Ozone` compara meses e
considera somente os dias medidos.

`mean()` calcula a média, `median()` a mediana, `sum()` o total, `sd()` o
desvio padrão, `min()` e `max()` os extremos. `dplyr::n()` conta linhas e
`dplyr::n_distinct()` conta valores diferentes.

> **Antes de continuar**
>
> O campo **Agrupar por** define quais observações são comparadas. Neste exemplo,
> `Month` forma uma linha para cada mês observado. Sem grupos, o bloco calcula
> as mesmas medidas para a tabela inteira. **Nome** e **Resumo** correspondem
> pela posição.

## Configuração

**Agrupar por** (`by`) lista os campos de agrupamento. **Nome** (`name`) e **Resumo** (`expr`) são listas correspondentes por posição; deixe `by` vazio para uma linha de resumo da tabela toda.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("mensal", "data/group_summarise", by = "Month",
         name = "ozonio_medio, dias",
         expr = "mean(Ozone, na.rm = TRUE), dplyr::n()", from = "ar")
```

A tabela resultante tem cinco linhas, uma por mês de `airquality`, com a média
de `Ozone` entre os dias medidos e o total de dias registrados naquele mês.

## Como interpretar

A média, os extremos e a contagem aparecem em colunas separadas. O grupo é
`Month`, codificado de 5 a 9 no conjunto `airquality`; ele representa o mês
no ano de 1973.

`na.rm = TRUE` calcula a média de `Ozone` apenas para os dias medidos; a coluna
`dias` inclui também os dias sem medição. Os dois resultados têm denominadores
diferentes, então `dias` não é a contagem usada pela média.
