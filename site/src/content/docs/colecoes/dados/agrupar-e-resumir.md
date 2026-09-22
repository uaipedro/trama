---
title: Agrupar e resumir
description: Produza uma medida por grupo sem perder de vista como cada valor foi calculado.
section: colecoes
collection: dados
node: data/group_summarise
category: agregar
order: 4
related: [data/summary, data/filter, view/points]
---

## Finalidade

`data/group_summarise` reduz várias linhas a uma linha por combinação de
grupos. Sem agrupamento, produz uma linha para a tabela inteira.

## Configuração

**Agrupar por** recebe as colunas que definem os grupos. **Nome** e **Resumo**
aceitam listas separadas por vírgula, correspondentes pela posição.

```r
tr_flow(reg) |>
  tr_add("regional", "data/group_summarise", by = "regiao",
         name = "receita, pedidos",
         expr = "sum(valor, na.rm = TRUE), dplyr::n()", from = "ler")
```

> **Antes de continuar**
>
> Uma expressão de resumo devolve um valor por grupo. `valor * 2` produz um
> valor por linha e, por isso, pertence a `data/mutate`, que preserva a
> quantidade de linhas.

## Condições de interpretação

Faltantes participam de `sum()` e `mean()` por padrão. `na.rm = TRUE` os
exclui, mas a medida passa a valer para as linhas restantes; essa condição deve
acompanhar a leitura do resultado.
