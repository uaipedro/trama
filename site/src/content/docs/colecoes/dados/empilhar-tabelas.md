---
title: Empilhar tabelas
description: Acrescente linhas de várias tabelas, alinhando colunas pelo nome.
section: colecoes
collection: dados
node: data/bind_rows
category: agregar
order: 26
related: [data/join, data/distinct]
---

## O que o bloco faz

Acrescenta linhas de várias tabelas e alinha as colunas pelos nomes; a sequência das entradas define a sequência dos blocos de linhas.

## Quando usar

Quando arquivos ou recortes de períodos diferentes têm as mesmas variáveis e precisam formar uma série de registros.

## Configuração

A porta **Tabelas** aceita ligações múltiplas. Conecte uma ou mais tabelas; a ordem das conexões determina a ordem dos registros empilhados.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("maio_junho", "data/filter", expr = "Month %in% c(5, 6)", from = "ar") |>
  tr_add("julho_agosto", "data/filter", expr = "Month %in% c(7, 8)", from = "ar") |>
  tr_add("verao", "data/bind_rows", from = c("maio_junho", "julho_agosto"))
```

## Como interpretar

O resultado junta os recortes de maio a agosto em uma tabela de 122 observações com as colunas originais de `airquality`. As linhas aparecem primeiro na ordem do recorte maio-junho e depois na do recorte julho-agosto.
