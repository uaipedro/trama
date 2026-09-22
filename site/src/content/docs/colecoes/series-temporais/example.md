---
title: Série de exemplo
description: "Carrega uma das séries temporais que vêm com o R."
section: colecoes
collection: series-temporais
node: series/example
category: Fonte
order: 2
related: [series/from_table, series/plot]
---

## O que o bloco faz

Carrega uma das séries temporais do pacote `datasets` do R. São exatamente os
conjuntos que o `data/example` recusa — lá eles não são tabela; aqui são o
assunto.

Algumas que valem conhecer:

- **AirPassengers** — passageiros aéreos mensais, 1949–1960. O caso de livro:
  tendência, sazonalidade, e sazonalidade que CRESCE com o nível (pede log).
- **co2** — CO₂ em Mauna Loa, mensal: tendência e sazonalidade estáveis.
- **Nile** — vazão anual do Nilo, com uma quebra em 1898. Sem sazonalidade.
- **UKgas** — consumo trimestral de gás no Reino Unido.
- **presidents** — aprovação trimestral dos presidentes dos EUA, com faltantes.
- **lynx** — capturas anuais de linces: ciclo de ~10 anos que não é sazonal.

A série chega com a frequência de origem — 12 para mensal, 4 para trimestral,
1 para anual — e é ela que os nós adiante usam para saber o que é "sazonal".

Séries múltiplas (`EuStockMarkets`, `Seatbelts`) aparecem uma vez por coluna,
como `EuStockMarkets$DAX`: o tipo `series/ts` guarda uma série só.

## Quando usar

Explore rapidamente uma série temporal incluída no R para reproduzir análises e conhecer comportamentos como tendência, sazonalidade, quebras e ciclos.

## Configuração

- **Série** — qual série carregar. A lista é derivada do R que está rodando.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example", dataset = "AirPassengers") |>
  tr_add("log", "series/transform", metodo = "log", from = "pax")
```

## Como interpretar

Uma série (`series/ts`). O card mostra o gráfico dela, e o resumo diz início,
fim, frequência e quantos faltantes. Ligada a qualquer nó da coleção `data`, a
série vira tabela com as colunas `tempo` e `valor` sozinha, sem nó no meio.

## Veja também

`series/from_table` para montar a série a partir de uma tabela sua;
`data/example` para os conjuntos que são tabela; `series/plot` para desenhá-la
com título e proporção escolhidos.
