---
title: Componente
description: "Tira um componente da decomposição como série."
section: colecoes
collection: series-temporais
node: series/component
category: Decompor
order: 2
related: [series/stl, series/decompose, series/regression]
---

## O que o bloco faz

Devolve um dos componentes de uma decomposição como série, para seguir
adiante: modelar a dessazonalizada, testar se o resto é ruído branco, desenhar
só a tendência.

**dessazonalizada** é a série sem o efeito sazonal — o número que se publica
quando se diz "descontado o efeito do mês". Na decomposição multiplicativa ela
é a série DIVIDIDA pelo sazonal (subtrair um fator de 1,2 de um valor na casa
das centenas não tiraria sazonalidade nenhuma); na aditiva, a série menos o
sazonal. O nó sabe qual das duas pela decomposição que recebeu.

**sem_tendencia** é o espelho: a série sem a tendência, com a sazonalidade
dentro — `série − tendência` na aditiva, `série / tendência` na multiplicativa.
É o "estimo a tendência e subtraio" às claras: com `series/regression` de grau 1
na frente, é a série menos a reta ajustada por mínimos quadrados.

**regressor** é o efeito da covariável (β·x) de uma `series/regression` com
a entrada `regressor` ligada. Numa decomposição sem regressor (STL, clássica,
regressão sem covariável) a escolha para o nó em vermelho, dizendo por quê.
Com regressor, a tendência é só a do tempo, e `sem_tendencia` e
`dessazonalizada` mantêm o efeito do regressor dentro.

Da clássica, tendência e resto (e, por isso, `sem_tendencia`) chegam com NA
nas pontas.

## Quando usar

Extraia tendência, sazonalidade, resto, série dessazonalizada ou série sem tendência para inspecionar ou usar em outra etapa. A entrada deve ser uma decomposição compatível.

## Configuração

- **Componente** — `tendencia`, `sazonal`, `resto`, `dessazonalizada`,
  `sem_tendencia` ou `regressor`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("stl", "series/stl", from = "pax") |>
  tr_add("resto", "series/component", componente = "resto", from = "stl") |>
  tr_add("rb", "series/ljung_box", from = "resto")
```

## Como interpretar

Uma série (`series/ts`) do mesmo tamanho da original.

## Veja também

`series/stl` e `series/decompose`, que produzem a decomposição;
`series/regression`, que também produz uma — estimada, com coeficiente e
p-valor por componente; `series/ljung_box` para testar o resto.
