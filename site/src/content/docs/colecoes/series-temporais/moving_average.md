---
title: Média móvel
description: "Suaviza a série com a média de k períodos vizinhos."
section: colecoes
collection: series-temporais
node: series/moving_average
category: Operar
order: 2
related: [series/decompose, series/stl, series/plot]
---

## O que o bloco faz

Troca cada valor pela média dos k vizinhos. É a forma mais simples de ver a
TENDÊNCIA: com k igual à frequência (12 numa série mensal), cada média cobre
um ano inteiro, e a sazonalidade some por construção.

Centrada com ordem par é a média 2×k — a média de duas médias de k defasadas
de um período —, que é o jeito de pôr o resultado no MEIO da janela quando a
janela não tem meio. É a mesma que a decomposição clássica usa por dentro.

Não centrada, a média de cada ponto usa só o passado (os k anteriores): é a
que se pode calcular em tempo real, mas ela ATRASA meia janela em relação à
série.

As pontas saem em branco (NA): meia janela no começo e no fim não tem vizinhos
para a média. É o honesto, e o gráfico mostra a linha começando depois.

## Quando usar

Suavize oscilações de curto prazo para destacar o movimento local da série. A ordem define quantas observações entram em cada média.

## Configuração

- **Ordem (k)** — quantos períodos entram em cada média.
- **Centrada** — liga a média no meio da janela (2×k para k par); desligada,
  a média usa os k períodos anteriores.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("co2", "series/example", dataset = "co2") |>
  tr_add("tend", "series/moving_average", ordem = 12L, from = "co2")
```

## Como interpretar

Uma série (`series/ts`) do mesmo tamanho, com NA nas pontas.

## Veja também

`series/decompose` e `series/stl`, que separam a tendência junto com os outros
componentes; `series/plot` para ver o resultado.
