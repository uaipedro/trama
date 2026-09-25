---
title: Operar duas séries
description: "Subtrai, soma, divide ou multiplica duas séries, alinhadas pelo tempo."
section: colecoes
collection: series-temporais
node: series/combine
category: Operar
order: 2
related: [series/detrend, series/component, series/aggregate]
---

## O que o bloco faz

Faz a conta ponto a ponto entre duas séries: `a − b`, `a + b`, `a / b` ou
`a × b`. Serve para tirar de uma série outra que se estimou à parte (a
tendência, um índice de preços), para somar partes, para calcular uma razão
entre duas medidas no mesmo tempo.

### Alinhamento pelo tempo

As séries são alinhadas pelo CALENDÁRIO, e não pela posição: se `a` vai de
1949 a 1960 e `b` de 1955 a 1965, a saída vai de 1955 a 1960 — o período em
comum. Séries sem período em comum param o nó em vermelho.

As duas têm de ter a mesma frequência. Mensal com trimestral é erro, e não
conversão implícita: agregar pede escolher como (soma? média?), e é o
`series/aggregate` que faz essa escolha às claras.

### Divisão por zero

Onde `b` é zero, `a / b` sai **em branco (NA)**, e não infinito: um infinito no
meio da série quebra a escala do gráfico e os nós seguintes. O resumo do card
conta os faltantes, e `series/interpolate` pode preenchê-los se fizer sentido.

## Quando usar

Quando a segunda série vem de outro caminho do fluxo — uma tendência estimada
por regressão, uma série de referência, um deflator — e a conta entre as duas
precisa respeitar o calendário.

## Configuração

- **Operação** — `a - b`, `a + b`, `a / b` ou `a * b`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("reg", "series/regression", grau = 1L, from = "pax") |>
  tr_add("tend", "series/component", componente = "tendencia", from = "reg") |>
  tr_add("sem", "series/combine", operacao = "a - b", from = "pax") |>
  tr_link("tend", "sem:b")
```

## Como interpretar

Uma série (`series/ts`) no período em comum das duas entradas.

## Veja também

`series/detrend`, que estima e tira a tendência num nó só;
`series/component` com `sem_tendencia`; `series/aggregate` para igualar as
frequências; `series/window` para escolher o período à mão.
