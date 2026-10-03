---
title: Criar coluna
description: Calcule colunas novas ou substitua colunas existentes preservando as linhas.
section: colecoes
collection: dados
node: data/mutate
category: transformar
order: 19
related: [data/group_summarise, data/convert]
---

## O que o bloco faz

Avalia cada expressão para as linhas da tabela e cria ou substitui a coluna nomeada sem agregar os registros.

## Quando usar

Quando cada registro precisa de uma medida derivada, como margem ou conversão de unidade.

## Configuração

**Nome** (`name`) e **Expressão** (`expr`) são listas correspondentes pela posição. **Por grupo** (`by`) opcional avalia cada cálculo dentro dos grupos.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("carros", "data/example", dataset = "mtcars") |>
  tr_add("potencia", "data/mutate", name = "potencia_peso",
         expr = "hp / wt", from = "carros")
```

## Como interpretar

`potencia_peso` contém `hp / wt` para cada modelo; a tabela continua com as 32 linhas de `mtcars` e ganha essa coluna calculada.
