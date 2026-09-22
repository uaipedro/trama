---
title: Juntar tabelas
description: Combine duas tabelas lado a lado usando colunas-chave.
section: colecoes
collection: dados
node: data/join
category: agregar
order: 25
related: [data/get_dupes, data/bind_rows]
---

## O que o bloco faz

Relaciona as tabelas das entradas esquerda e direita pelas chaves indicadas e combina suas colunas nas linhas correspondentes.

## Quando usar

Quando duas fontes descrevem entidades relacionadas e compartilham uma chave.

## Configuração

**Por** (`by`) lista as chaves compartilhadas; **Tipo** (`type`) define `inner`, `left`, `right`, `full` ou `anti`. A entrada esquerda e a direita são conectadas nessa ordem.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("vendas", "data/generate", n = 3L,
         expr = "cliente_id = c(1, 1, 2), valor = c(10, 15, 8)") |>
  tr_add("clientes", "data/generate", n = 2L,
         expr = "cliente_id = c(1, 2), regiao = c('Norte', 'Sul')") |>
  tr_add("detalhe", "data/join", by = "cliente_id",
         from = c("vendas", "clientes")) |>
  tr_set("detalhe", type = "left")
```

## Como interpretar

A junção à esquerda mantém as três vendas; cada linha recebe a região do cadastro pela chave `cliente_id`. Chaves repetidas no cadastro multiplicariam linhas e devem ser verificadas antes da análise.
