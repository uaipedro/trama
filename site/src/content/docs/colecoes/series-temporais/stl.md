---
title: Decomposição STL
description: "Decomposição por suavização local: sazonalidade que muda devagar, robusta a outlier."
section: colecoes
collection: series-temporais
node: series/stl
category: Decompor
order: 2
related: [series/transform, series/decompose, series/regression]
---

## O que o bloco faz

Separa tendência, sazonalidade e resto por regressão local (loess) — o
método STL de Cleveland et al. (1990). Contra a clássica, ele:

- deixa a sazonalidade MUDAR com os anos, devagar;
- não perde as pontas: tendência e resto existem na série inteira;
- com **Robusta**, não deixa um mês estranho (uma greve, um apagão) puxar a
  sazonalidade de todos os outros anos — o mês vai parar no resto, que é onde
  se procura por ele.

### Janela sazonal

É quantos anos entram na suavização de cada estação. **0 é "periódica"**: o
mesmo padrão em todos os anos, como na clássica. Um número ímpar maior ou
igual a 7 deixa o padrão variar — quanto menor, mais depressa. 7 é o mínimo
que o método aceita; 13 ou 21 são escolhas comuns para séries longas.

A STL é só aditiva. Para sazonalidade multiplicativa, transforme com log antes
(`series/transform`) e a decomposição do log é aditiva.

Pede série sazonal, com pelo menos dois ciclos, e não aceita faltantes.

## Quando usar

Separe tendência, sazonalidade e resto por suavização local quando o padrão sazonal pode mudar gradualmente. A opção robusta reduz a influência de valores atípicos.

## Configuração

- **Janela sazonal** — 0 para periódica; senão, ímpar maior ou igual a 7.
- **Robusta** — pesos que ignoram outlier na estimação.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("co2", "series/example", dataset = "co2") |>
  tr_add("stl", "series/stl", janela_sazonal = 13L, robusta = TRUE, from = "co2") |>
  tr_add("resto", "series/component", componente = "resto", from = "stl") |>
  tr_add("acf", "series/acf", from = "resto")
```

## Como interpretar

Uma decomposição (`series/decomposition`), com a força de tendência e de
sazonalidade no resumo do card.

## Veja também

`series/decompose` para a clássica; `series/regression` para a paramétrica, a
única que dá coeficiente e p-valor por componente; `series/transform` para
decompor o log; `series/component` para extrair um componente;
`series/interpolate` quando a série tem faltantes.
