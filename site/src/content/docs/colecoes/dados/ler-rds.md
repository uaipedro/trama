---
title: Ler RDS
description: Recupere de um arquivo RDS uma tabela gravada no R.
section: colecoes
collection: dados
node: data/read_rds
category: fonte
order: 5
related: [data/write_rds, data/summary]
---

## O que o bloco faz

Carrega a tabela gravada em RDS e preserva classes e atributos compatíveis com R.

## Quando usar

Quando outro fluxo ou sessão do R gravou a tabela e você quer recuperar seus tipos e atributos.

## Configuração

**Arquivo** (`path`) indica o caminho do objeto `.rds`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

arquivo <- tempfile(fileext = ".rds")
saveRDS(datasets::airquality, arquivo)
tr_flow(reg) |>
  tr_add("ler", "data/read_rds", path = arquivo) |>
  tr_add("perfil", "data/summary", from = "ler")
```

## Como interpretar

O objeto é lido e normalizado como tabela pelo contrato `data/table`. Use `data/write_rds` para produzir esse formato.
