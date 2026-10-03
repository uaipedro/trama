---
title: Decomposição clássica
description: "Separa tendência, sazonalidade e resto por médias móveis."
section: colecoes
collection: series-temporais
node: series/decompose
category: Decompor
order: 2
related: [series/component, series/stl, series/plot_decomposition]
---

## O que o bloco faz

Separa a série em três componentes, pelo método que se ensina primeiro:

1. a TENDÊNCIA é a média móvel centrada de ordem igual ao ciclo;
2. o SAZONAL é a média, estação por estação, do que sobra da série sem a
   tendência — um valor para cada mês, repetido todos os anos;
3. o RESTO é o que sobra.

Na **aditiva**, `série = tendência + sazonal + resto`: o efeito de dezembro é
"+40 passageiros". Na **multiplicativa**, `série = tendência × sazonal ×
resto`: o efeito de dezembro é "×1,2" — e é a certa quando a oscilação cresce
com o nível, como em `AirPassengers`.

A clássica tem dois limites que a STL não tem: o sazonal é IDÊNTICO em todos os
anos, e as pontas da tendência e do resto saem em branco (meio ciclo em cada
lado). Por isso a STL é a de uso; esta é a de entender.

Pede série sazonal (frequência maior que 1), com pelo menos dois ciclos.
O bloco não aceita faltantes, e a multiplicativa pede valores positivos.

O resumo do card traz a FORÇA da tendência e da sazonalidade, de 0 a 1
(Hyndman & Athanasopoulos): acima de ~0,6, o componente é real; perto de 0, é
ruído.

## Quando usar

Separe uma série com sazonalidade estável em tendência, componente sazonal e resto usando médias móveis. Escolha o tipo aditivo ou multiplicativo conforme a amplitude sazonal permaneça constante ou cresça com o nível.

## Configuração

- **Tipo** — `aditiva` ou `multiplicativa`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("dec", "series/decompose", tipo = "multiplicativa", from = "pax") |>
  tr_add("dessaz", "series/component", componente = "dessazonalizada", from = "dec")
```

## Como interpretar

Uma decomposição (`series/decomposition`): o card mostra os quatro painéis.
`series/component` tira um componente como série; ligada a um nó da `data`,
vira tabela com `tempo`, `observado`, `tendencia`, `sazonal` e `resto`.

## Veja também

`series/stl`, a decomposição que se usa na prática; `series/component` para
extrair um componente; `series/plot_decomposition` para escolher proporção e
título do gráfico; `series/regression`, que estima os mesmos componentes e, por
estimá-los, dá coeficiente e p-valor a cada um.
