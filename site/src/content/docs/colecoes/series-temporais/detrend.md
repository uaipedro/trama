---
title: Tirar tendência
description: "Estima a tendência e a subtrai da série, mantendo a sazonalidade."
section: colecoes
collection: series-temporais
node: series/detrend
category: Operar
order: 2
related: [series/component, series/combine, series/diff, series/moving_average]
---

## O que o bloco faz

Estima a tendência da série e a TIRA, deixando o resto — sazonalidade
incluída. É o "ajusto uma reta e subtraio" feito num nó só, com a tendência
escolhida às claras:

- **linear** — a reta de mínimos quadrados em t. A saída tem média zero e
  inclinação zero por construção.
- **polinomial** — um polinômio de grau 2 a 5 em t, para tendência que curva.
  Graus altos acompanham demais as pontas; comece pelo 2.
- **loess** — regressão local, sem forma imposta. A **Suavidade** é a fração
  da série que entra em cada ajuste local: perto de 1, uma curva larga; perto
  de 0,1, uma que segue até a sazonalidade (e aí a tira junto).
- **diferenca** — `x[t] − x[t−1]`, sem modelo nenhum. A saída é outra
  grandeza: a VARIAÇÃO de um período para o seguinte, e não a série em torno
  da tendência. A série perde a 1ª observação e começa um período depois. É a
  mesma conta de `series/diff` simples; está aqui para comparar com os outros
  métodos no mesmo card.

A tendência estimada vai junto com a série, como atributo `tendencia` —
no console, `attr(saida, "tendencia")` —, e `saída + tendência` devolve a
série original.

Nos três métodos com modelo (linear, polinomial, loess), a sazonalidade FICA
na saída; na diferença ela sobra só como variação entre meses vizinhos, com
outra forma. Para tirar as duas, use `series/component` com
`resto`; para tirar só a sazonalidade, `dessazonalizada`. A reta daqui é
estimada sem olhar a sazonalidade; com anos completos, é a mesma de
`series/regression` de grau 1, e com anos incompletos difere um pouco — lá a
reta e as dummies sazonais são estimadas juntas.

Faltantes não impedem o ajuste (exceto na diferença, onde propagam): a
tendência é estimada com os valores observados, e a saída tem NA onde a série
tinha.

## Quando usar

Quando a pergunta é sobre as oscilações em torno da tendência — a sazonalidade,
os ciclos, o resto — e a tendência só atrapalha a leitura. Também antes de um
correlograma: a tendência domina a autocorrelação e esconde o resto.

## Configuração

- **Método** — `linear`, `polinomial`, `loess` ou `diferenca`.
- **Grau (polinomial)** — de 2 a 5. Só vale para `polinomial`.
- **Suavidade (loess)** — maior que 0 e até 1. Só vale para `loess`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("sem", "series/detrend", metodo = "linear", from = "pax") |>
  tr_add("g", "series/plot", from = "sem")
```

## Como interpretar

Uma série (`series/ts`) sem a tendência, no mesmo tempo da original (um
período mais curta na diferença), com a tendência estimada no atributo
`tendencia`.

## Veja também

`series/component` com `sem_tendencia`, para a mesma conta a partir de uma
decomposição; `series/combine` para subtrair uma tendência estimada em outro
lugar; `series/diff` para diferenças de ordem maior ou sazonais;
`series/moving_average` para ver a tendência sem tirá-la.
