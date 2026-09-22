---
title: Dados de exemplo
description: Carregue uma tabela incluída no R sem depender de arquivo externo.
section: colecoes
collection: dados
node: data/example
category: fonte
order: 8
related: [data/summary, data/generate]
---

## O que o bloco faz

Carrega um conjunto tabular do pacote `datasets` instalado com R. Nomes de linha com identificadores informativos, como modelos de carro em `mtcars`, viram a coluna `nome`.

## Quando usar

Use para explorar transformações com dados reproduzíveis já disponíveis na instalação do R.

## Configuração

**Conjunto** (`dataset`) seleciona uma tabela disponível em `datasets`. A lista depende da versão do R instalada.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("carros", "data/example", dataset = "mtcars") |>
  tr_add("consumo", "data/select", cols = "nome, mpg, cyl, wt", from = "carros")
```

## Como interpretar

Cada linha representa um carro; identificadores guardados nos nomes das linhas são preservados na coluna `nome` quando têm conteúdo informativo.
