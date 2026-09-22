---
title: Gravar Parquet
description: Grave a tabela no formato colunar Parquet.
section: colecoes
collection: dados
node: data/write_parquet
category: saida
order: 31
related: [data/read_parquet, data/write_rds]
---

## O que o bloco faz

Escreve a tabela em Parquet, formato colunar, e encaminha a mesma tabela para as etapas seguintes.

## Quando usar

Para armazenar ou compartilhar tabelas grandes em um formato colunar; requer `arrow`.

## Configuração

**Arquivo** (`path`) define o destino do arquivo `.parquet`; gravar Parquet requer o pacote opcional `arrow`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

arquivo <- tempfile(fileext = ".parquet")
tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("saida", "data/write_parquet", path = arquivo, from = "ar")
```

## Como interpretar

O nó grava e repassa a tabela. `data/read_parquet` lê o resultado de volta.
