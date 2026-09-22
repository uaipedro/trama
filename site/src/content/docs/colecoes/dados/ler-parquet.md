---
title: Ler Parquet
description: Leia uma tabela colunar Parquet para o fluxo.
section: colecoes
collection: dados
node: data/read_parquet
category: fonte
order: 6
related: [data/write_parquet, data/summary]
---

## O que o bloco faz

Lê uma tabela Parquet e a disponibiliza no fluxo como `data/table`.

## Quando usar

Quando a origem está em Parquet, especialmente em conjuntos grandes ou compartilhados entre ferramentas.

## Configuração

**Arquivo** (`path`) indica o arquivo `.parquet`. A leitura requer o pacote opcional `arrow`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

arquivo <- tempfile(fileext = ".parquet")
arrow::write_parquet(datasets::airquality, arquivo)
tr_flow(reg) |>
  tr_add("ler", "data/read_parquet", path = arquivo) |>
  tr_add("perfil", "data/summary", from = "ler")
```

## Como interpretar

As colunas são lidas como tabela. `data/summary` permite conferir tipos e faltantes.
