---
title: Interpolar faltantes
description: "Preenche os faltantes pela tendência e pela sazonalidade da série."
section: colecoes
collection: series-temporais
node: series/interpolate
category: Operar
order: 2
related: [series/window, series/aggregate, series/decompose]
---

## O que o bloco faz

Estima os valores faltantes a partir da própria série: numa série sem
sazonalidade, por interpolação linear entre os vizinhos; numa sazonal, pela
decomposição STL — o julho faltante sai com cara de julho, a partir da
tendência daquele ano e do padrão dos outros julhos (`forecast::na.interp`).

Existe ao lado do `data/replace_na` porque faz outra coisa. Aquele põe uma
CONSTANTE, e numa série um zero no lugar do julho derruba a sazonalidade
inteira que os nós seguintes vão estimar. Este nó estima.

Decomposição, testes de raiz unitária e Holt-Winters recusam série com
faltante, apontando para cá.

Série sem faltante passa intacta.

## Quando usar

Preencha períodos ausentes quando uma etapa seguinte exige uma série completa. A escolha do método define quais padrões locais e sazonais são preservados.

## Configuração

Nenhum.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pres", "series/example", dataset = "presidents") |>
  tr_add("cheia", "series/interpolate", from = "pres") |>
  tr_add("stl", "series/stl", from = "cheia")
```

## Como interpretar

Uma série (`series/ts`) do mesmo tamanho, sem faltantes.

## Veja também

`data/replace_na` para preencher com constante; `series/window` para cortar um
trecho com buraco em vez de inventá-lo.
