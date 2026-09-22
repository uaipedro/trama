---
title: Gravar RDS
description: Grave um objeto tabular em arquivo RDS para recuperar no R.
section: colecoes
collection: dados
node: data/write_rds
category: saida
order: 30
related: [data/read_rds, data/write_csv]
---

## O que o bloco faz

Serializa a tabela em RDS no caminho indicado e encaminha a mesma tabela para as etapas seguintes.

## Quando usar

Para persistir uma tabela mantendo classes e atributos próprios do R.

## Configuração

**Arquivo** (`path`) define o destino do arquivo `.rds`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

arquivo <- tempfile(fileext = ".rds")
tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("saida", "data/write_rds", path = arquivo, from = "ar")
```

## Como interpretar

O nó grava e repassa a tabela. `data/read_rds` recupera o arquivo em outro fluxo.
