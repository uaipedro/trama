---
title: Juntar colunas
description: Junte várias colunas numa só, com um separador.
section: colecoes
collection: dados
node: data/unite
category: transformar
order: 24
related: [data/separate]
---

## O que o bloco faz

Cola os valores das **Colunas** (`cols`), na ordem dada, com o **Separador** entre eles, numa coluna **Nome** (`nome`) posta onde estava a primeira. Faltante vira o texto `NA`.

## Exemplo

```r
tr_add("data", "data/unite", cols = "dia, mes, ano", nome = "data", separador = "/", from = "ler")
```
