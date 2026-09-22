---
title: Limpar nomes
description: Padronize nomes de colunas em uma forma simples de referenciar nas expressões.
section: colecoes
collection: dados
node: data/clean_names
category: limpar
order: 11
related: [data/rename, data/read_excel]
---

## O que o bloco faz

Normaliza os nomes das colunas para minúsculas e separadores com sublinhado, facilitando seu uso em expressões R.

## Quando usar

Depois de importar planilhas ou arquivos com espaços, acentos e convenções inconsistentes nos cabeçalhos.

## Configuração

O bloco não tem parâmetros; normaliza todos os nomes de coluna da entrada.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("entrada", "data/generate",
         expr = "tibble::tibble('Valor Total' = c(10, 12), 'Área (ha)' = c(2, 3))") |>
  tr_add("nomes", "data/clean_names", from = "entrada")
```

## Como interpretar

A saída contém as colunas `valor_total` e `area_ha`; os valores 10, 12, 2 e 3 permanecem associados às mesmas linhas.
