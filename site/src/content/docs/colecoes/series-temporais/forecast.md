---
title: Prever
description: "Prevê h períodos à frente com um modelo ajustado, com intervalos de 80 e 95%."
section: colecoes
collection: series-temporais
node: series/forecast
category: Modelar
order: 2
related: [series/residuals, series/ljung_box, series/arima]
---

## O que o bloco faz

Projeta o modelo **Horizonte** períodos à frente, com a previsão pontual e os
intervalos de 80% e 95%.

É um nó separado do ajuste por causa do custo: ajustar é o caro (um ARIMA
automático testa dezenas de modelos), prever é aritmética. Trocar o horizonte
de 12 para 24 recomputa só este card.

Os intervalos são o ponto. Uma previsão sem leque diz "vai dar 450"; com o
leque, diz "entre 390 e 520 com 95% de chance" — e o leque ABRE com o
horizonte, que é a informação mais honesta que um modelo dá sobre o próprio
limite. O que eles supõem está em Pressupostos, logo abaixo.

Os níveis são sempre 80 e 95, porque são os que o gráfico e a tabela nomeiam
(`li_80`, `ls_95`); um fluxo que filtra por `ls_95` não quebra.

Série transformada é prevista na escala transformada.

### Intervalo: normal ou bootstrap

- **normal** (padrão) — os limites são quantis normais em torno da previsão,
  com a variância do modelo. Supõe resíduos normais.
- **bootstrap** — simula 5000 trajetórias futuras reamostrando os resíduos do
  ajuste e toma os quantis empíricos (Hyndman & Athanasopoulos, FPP3). Não
  supõe normalidade, só que os resíduos sejam independentes e de variância
  constante. Usa a semente do nó: o mesmo fluxo dá o mesmo leque. Só para
  `series/arima` e `series/ets`; com `series/holt_winters` o bloco recusa.

## Quando usar

Gere valores futuros a partir de um modelo ajustado e escolha o horizonte na unidade temporal da série. Os intervalos expressam incerteza crescente com o horizonte.

## Configuração

- **Horizonte** — quantos períodos prever.
- **Intervalo** — `normal` (padrão) ou `bootstrap`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("ets", "series/ets", from = "pax") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "ets") |>
  tr_add("csv", "data/write_csv", path = "previsao.csv", from = "prev")
```

## Como interpretar

Uma previsão (`series/forecast`): o card mostra histórico e leque. Ligada a um
nó da `data`, vira tabela com `tempo`, `previsto`, `li_80`, `ls_80`, `li_95` e
`ls_95` — é por aí que se grava num CSV.

## Veja também

`series/arima`, `series/ets` e `series/holt_winters` para o modelo;
`series/baseline` para a referência; `series/accuracy` para o erro;
`series/plot_forecast` para escolher o histórico mostrado; `data/write_csv`
para exportar.
