---
title: Gravar CSV
description: Grave a tabela em arquivo delimitado e continue o fluxo com a mesma tabela.
section: colecoes
collection: dados
node: data/write_csv
category: saida
order: 29
related: [data/read_csv, data/write_rds]
---

## O que o bloco faz

Escreve a tabela em CSV no caminho indicado e encaminha a mesma tabela para as etapas seguintes.

## Quando usar

Para entregar dados em formato aberto que planilhas e outras ferramentas possam ler.

## Configuração

**Arquivo** (`path`) define o destino. O arquivo é separado por vírgulas.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

arquivo <- tempfile(fileext = ".csv")
tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("saida", "data/write_csv", path = arquivo, from = "ar")
```

## Como interpretar

O arquivo contém a tabela de entrada. A saída do nó também repassa essa tabela, permitindo encadear outras operações.
