---
title: Descritivas
description: Média, desvio, mediana, extremos, assimetria e curtose de cada coluna numérica.
section: colecoes
collection: dados
node: data/describe
category: conhecer
order: 3
related: [data/summary, data/group_summarise, models/jarque_bera]
---

## O que o bloco faz

`data/describe` devolve uma linha por coluna numérica, com `n`, `faltantes`, `media`, `dp`, `mediana`, `minimo`, `maximo`, `assimetria` e `curtose_excesso`. Faltantes ficam de fora de cada conta e são contados à parte.

## Quando usar

Para citar as descritivas num relatório: o `data/summary` mostra o que chegou em cada coluna, e este bloco mostra a distribuição das numéricas.

## Configuração

**Colunas** (`cols`) — colunas numéricas separadas por vírgula. Em branco, todas as numéricas, menos o `tempo` de uma série que chegou pelo adaptador da coleção de séries.

Duas convenções mudam o número:

- `dp` é o desvio-padrão amostral, com divisor n − 1.
- `assimetria` e `curtose_excesso` são os coeficientes de momento g1 = m3/m2^(3/2) e g2 = m4/m2² − 3, com divisor n (Joanes e Gill, 1998). A curtose é em excesso: zero na normal.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("flor", "data/example", dataset = "iris") |>
  tr_add("desc", "data/describe", from = "flor")
```

## Como interpretar

Assimetria positiva indica cauda mais longa à direita; curtose em excesso positiva, caudas mais pesadas que as da normal. Coluna constante sai com as duas em branco.
