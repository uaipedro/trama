---
title: Selecionar colunas
description: Mantenha as colunas indicadas ou retire-as da tabela.
section: colecoes
collection: dados
node: data/select
category: transformar
order: 20
related: [data/rename, data/pivot_longer]
---

## O que o bloco faz

Mantém as colunas selecionadas ou remove apenas essas colunas quando **Remover em vez de manter** está ligado.

## Quando usar

Para preparar a tabela com as variáveis necessárias à análise ou reduzir campos auxiliares.

## Configuração

**Colunas** (`cols`) lista os campos. **Remover em vez de manter** (`remove`) inverte a seleção quando ativado.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("carros", "data/example", dataset = "mtcars") |>
  tr_add("essenciais", "data/select", cols = "nome, mpg, wt", from = "carros")
```

## Como interpretar

A saída tem 32 linhas e apenas `nome`, `mpg` e `wt`, na ordem em que os campos foram listados.
