---
title: Transformar
description: "Log, raiz ou Box-Cox: estabiliza a variância que cresce com o nível."
section: colecoes
collection: series-temporais
node: series/transform
category: Operar
order: 2
related: [series/decompose, series/ets]
---

## O que o bloco faz

Aplica uma transformação que ACHATA a variação quando ela cresce com o nível
da série. Em `AirPassengers` a oscilação de cada ano é maior que a do anterior
porque a sazonalidade é multiplicativa; depois do log, as oscilações ficam do
mesmo tamanho, e decomposição aditiva, ARIMA e ETS aditivo passam a servir.

- **log** — a mais comum, e a que se interpreta: diferença de log é variação
  percentual (aproximada).
- **raiz** — mais branda que o log; aceita zero.
- **boxcox** — uma família que vai da identidade (λ = 1) ao log (λ = 0). Com
  **λ** em branco, o valor é escolhido pelo método de Guerrero, e fica
  guardado na série.

Log e Box-Cox exigem valores positivos, e raiz exige não negativos: zero ou
negativo para o nó em vermelho, dizendo quantos há. O conserto não é um param
aqui — é somar uma constante antes, num `data/mutate`, escolhendo às claras
quanto somar.

A previsão de uma série transformada sai na escala transformada. Para voltar,
leve a tabela da previsão a um `data/mutate` com `exp()`.

## Quando usar

Estabilize a variância quando a amplitude das oscilações cresce com o nível. A escolha entre log, raiz e Box–Cox depende da escala e dos valores da série.

## Configuração

- **Método** — `log`, `raiz` ou `boxcox`.
- **λ (Box-Cox)** — só vale para `boxcox`. Vazio escolhe automaticamente;
  um número (`0.5`, ou `0,5`) fixa. Ignorado nos outros métodos.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("bc", "series/transform", metodo = "boxcox", lambda = "", from = "pax")
```

## Como interpretar

Uma série (`series/ts`) do mesmo tamanho, na escala transformada.

## Veja também

`series/decompose` e `series/ets`, que têm versão multiplicativa quando não se
quer transformar; `data/mutate` para somar uma constante antes, ou para voltar
da escala com `exp()`.
