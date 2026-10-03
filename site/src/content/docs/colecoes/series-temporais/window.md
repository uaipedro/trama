---
title: Recortar período
description: "Mantém só os períodos entre um início e um fim."
section: colecoes
collection: series-temporais
node: series/window
category: Operar
order: 2
related: [series/accuracy, series/transform, series/moving_average]
---

## O que o bloco faz

Recorta a série no tempo. O uso mais importante é separar TREINO de TESTE:
um recorte até 1958 alimenta o modelo, e a série inteira vai ao
`series/accuracy` para medir o erro nos anos que o modelo não viu.

Os períodos se escrevem como `ano` ou `ano, período`: `1955, 3` é março de
1955 numa série mensal, o terceiro trimestre numa trimestral.

**Fim só com o ano vai até o último período daquele ano**: `fim = 1958` numa
série mensal para em dezembro de 1958. (O `window()` do R pararia em janeiro,
que não é o que ninguém quer dizer.) **Início** só com o ano começa no primeiro
período.

Período fora da série para o nó em vermelho, dizendo de quando a quando ela
vai — em vez de recortar em silêncio até a borda, o que faria um `1995`
digitado no lugar de `1959` devolver a série inteira.

Os dois em branco desligam o nó: a série passa inteira.

## Quando usar

Selecione um intervalo contínuo da série para definir treino, teste ou período de análise. Os limites usam o calendário carregado pela série.

## Configuração

- **Início** — primeiro período mantido. Vazio é o começo da série.
- **Fim** — último período mantido. Vazio é o fim da série.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("treino", "series/window", fim = "1958", from = "pax") |>
  tr_add("modelo", "series/ets", from = "treino") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "modelo") |>
  tr_add("erro", "series/accuracy", from = "prev") |>
  tr_link("pax", "erro:real")
```

## Como interpretar

Uma série (`series/ts`) mais curta.

## Veja também

`series/accuracy`, que usa o recorte para medir erro fora da amostra;
`data/filter`, para recortar a tabela por data quando a série já virou tabela.
