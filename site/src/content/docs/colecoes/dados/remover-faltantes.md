---
title: Remover faltantes
description: Descarte linhas que têm faltantes nas colunas escolhidas.
section: colecoes
collection: dados
node: data/drop_na
category: limpar
order: 15
related: [data/replace_na, data/summary]
---

## O que o bloco faz

Retém as linhas em que todas as colunas selecionadas têm valor observado.

## Quando usar

Quando a análise requer valores presentes nessas variáveis e a exclusão é justificável.

## Configuração

**Colunas** (`cols`) lista as variáveis obrigatórias; uma linha sai se tiver `NA` em qualquer uma delas.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("com_ozonio", "data/drop_na", cols = "Ozone", from = "ar")
```

## Como interpretar

A saída contém somente os dias de `airquality` com medida de ozônio. As linhas sem `Ozone` são excluídas; os faltantes de outras colunas não determinam a remoção.
