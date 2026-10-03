---
title: Acurácia
description: "Medidas de erro da previsão: no treino e, com a série real, no teste."
section: colecoes
collection: series-temporais
node: series/accuracy
category: Modelar
order: 2
related: [series/window, series/baseline]
---

## O que o bloco faz

Calcula as medidas de erro de uma previsão:

- **rmse** — raiz do erro quadrático médio, na unidade da série. Pune erro
  grande.
- **mae** — erro absoluto médio, na unidade da série.
- **mape** — erro percentual absoluto médio. Explode perto de zero.
- **mase** — erro absoluto escalado pelo erro do ingênuo (sazonal) no treino.
  Abaixo de 1, o modelo bate a referência; acima, perde. É a medida que
  compara séries diferentes.
- **me**, **mpe** — erro médio e percentual médio: o VIÉS (sistematicamente
  acima ou abaixo).
- **acf1** — autocorrelação dos erros na defasagem 1.

### Treino e teste

Sem nada na entrada **real**, sai só a linha do TREINO: erro de um passo à
frente dentro da amostra, que é otimista por construção — o modelo viu esses
dados.

Com a série inteira ligada em **real** (e o modelo ajustado num recorte dela,
por `series/window`), sai também a linha do TESTE: o erro nos períodos que o
modelo não viu. É ela que diz se a previsão presta.

Série real que não cobre nenhum período da previsão para o nó em vermelho: é
quase sempre o fio ligado na série de treino por engano, e uma tabela só com o
treino pareceria resposta.

A tabela sai com o nome do método em cada linha: várias acurácias num
`data/bind_rows` viram a comparação de modelos.

## Quando usar

Compare modelos pela capacidade de prever períodos que não participaram do ajuste. Separe a série em treino e teste; a medida de treino sozinha tende a ser otimista.

## Configuração

Nenhum. Duas entradas: **previsao** (obrigatória) e **real** (opcional, a
série com os períodos previstos).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("treino", "series/window", fim = "1958", from = "pax") |>
  tr_add("ets", "series/ets", from = "treino") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "ets") |>
  tr_add("erro", "series/accuracy", from = "prev") |>
  tr_link("pax", "erro:real")
```

## Como interpretar

Uma tabela (`data/table`) com uma linha por conjunto (`treino`, `teste`) e as
colunas `metodo`, `conjunto`, `me`, `rmse`, `mae`, `mpe`, `mape`, `mase`,
`acf1` (e `theil_s_u` no teste).

## Veja também

`series/window` para separar treino e teste; `series/baseline` para a
referência; `data/bind_rows` para comparar vários modelos numa tabela.
