---
title: Ler JSON
description: Converta um JSON tabular em tabela para o fluxo.
section: colecoes
collection: dados
node: data/read_json
category: fonte
order: 4
related: [data/summary, data/read_csv]
---

## O que o bloco faz

Converte um array de objetos JSON em uma tabela: cada objeto passa a ser uma linha e cada propriedade, uma coluna.

## Quando usar

Quando o arquivo contém um array de objetos ou um objeto de vetores com comprimentos compatíveis.

## Configuração

**Arquivo** (`path`) indica o JSON. O conteúdo deve ser um array de objetos ou um objeto de vetores de mesmo comprimento.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

arquivo <- tempfile(fileext = ".json")
writeLines('[{"cliente":"Ana","valor":120},{"cliente":"Bia","valor":85}]', arquivo)
tr_flow(reg) |>
  tr_add("ler", "data/read_json", path = arquivo) |>
  tr_add("total", "data/group_summarise", by = "cliente", name = "vendas",
         expr = "sum(valor)", from = "ler")
```

## Como interpretar

Cada objeto do array vira uma linha; suas propriedades formam as colunas. JSON hierárquico que não representa uma tabela não é adequado a este nó.
