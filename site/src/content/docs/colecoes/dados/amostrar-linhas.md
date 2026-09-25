---
title: Amostrar linhas
description: Sorteie linhas com semente, por grupo, com ou sem reposição.
section: colecoes
collection: dados
node: data/sample
category: transformar
order: 26
related: [data/slice_head, data/filter]
---

## O que o bloco faz

Sorteia **N** (`n`) linhas, ou a **Fração** (`fracao`) quando N é 0, com a semente do bloco: a mesma amostra toda vez. **Com reposição** (`reposicao`) permite bootstrap. **Por grupo** (`grupo`) sorteia dentro de cada estrato.

## Exemplo

```r
tr_add("am", "data/sample", n = 5L, grupo = "tratamento", from = "ler")
```
