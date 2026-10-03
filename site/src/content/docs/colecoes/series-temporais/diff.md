---
title: Diferença
description: "Diferença simples (x[t] - x[t-1]) ou sazonal (x[t] - x[t-ciclo])."
section: colecoes
collection: series-temporais
node: series/diff
category: Operar
order: 2
related: [series/ndiffs, series/adf, series/kpss]
---

## O que o bloco faz

Troca cada valor pela variação em relação a um valor anterior. É a operação
que tira tendência (diferença simples) e sazonalidade (diferença sazonal) de
uma série, e o **I** do ARIMA.

- **simples**: `x[t] - x[t-1]`, a variação de um período para o seguinte.
  Tira tendência linear.
- **sazonal**: `x[t] - x[t-ciclo]`, a variação em relação ao mesmo período do
  ciclo anterior — janeiro contra janeiro. Tira sazonalidade estável. O ciclo é
  a frequência da série; numa série anual (frequência 1) não há ciclo, e o nó
  para em vermelho em vez de fazer uma diferença simples com outro nome.

**Ordem** repete a operação: ordem 2 é a diferença da diferença. Raramente
passa de 1; `series/ndiffs` diz quantas a série pede.

Cada diferença come observações do começo: uma simples come 1, uma sazonal
mensal come 12. É por isso que a série sai mais curta.

Para série com sazonalidade E tendência, o comum é encadear dois nós: sazonal
e depois simples (a ordem não altera o resultado).

## Quando usar

Remova tendência ou sazonalidade antes de modelar quando a série apresentar essas estruturas. A diferença sazonal compara períodos equivalentes e usa a frequência declarada.

## Configuração

- **Tipo** — `simples` (defasagem 1) ou `sazonal` (defasagem igual à
  frequência).
- **Ordem** — quantas vezes aplicar, de 1 a 3.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d12", "series/diff", tipo = "sazonal", from = "log") |>
  tr_add("d1", "series/diff", from = "d12") |>
  tr_add("adf", "series/adf", from = "d1")
```

## Como interpretar

Uma série mais curta (`series/ts`), começando depois das observações
consumidas.

## Veja também

`series/ndiffs` para saber quantas diferenças usar; `series/adf` e
`series/kpss` para conferir o resultado; `series/acf` para ver o que sobrou de
estrutura;
`series/arima`, que faz a diferença por dentro quando `d` ou `D` são maiores
que zero.
