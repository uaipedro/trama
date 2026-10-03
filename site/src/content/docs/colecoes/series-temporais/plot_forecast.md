---
title: Gráfico da previsão
description: "O histórico e a previsão, com os leques de 80 e 95%."
section: colecoes
collection: series-temporais
node: series/plot_forecast
category: Ver
order: 2
related: [series/forecast, series/baseline, series/accuracy]
---

## O que o bloco faz

O histórico em cinza, a previsão em cor, e os dois leques: o escuro é o
intervalo de 80%, o claro o de 95%. O leque ABRE com o horizonte — é a
incerteza crescendo, e a parte mais honesta do gráfico.

**Períodos de histórico** corta o passado mostrado: com 40 anos de série e 12
meses previstos, o leque vira um risco na ponta direita. Não muda a previsão,
que já foi feita no nó anterior — muda só o enquadramento.

O subtítulo diz o método (`ETS(M,Ad,M)`, `Seasonal naive method`).

## Quando usar

Apresente o histórico, a previsão e seus intervalos no mesmo eixo temporal. Confira se o horizonte e a escala estão claros para quem interpreta.

## Configuração

- **Períodos de histórico** — quantos períodos do passado mostrar. 0 mostra
  tudo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("ets", "series/ets", from = "pax") |>
  tr_add("prev", "series/forecast", horizonte = 24L, from = "ets") |>
  tr_add("g", "series/plot_forecast", historico = 48L, from = "prev")
```

## Como interpretar

Um gráfico (`view/plot`).

## Veja também

`series/forecast` e `series/baseline` para a previsão; `series/accuracy` para
o erro.

### Aparência (comum a todos os gráficos)

- **Proporção** — a forma da imagem: `16:9` e `2:1` para paisagem, `1:1` para
  quadrado, `4:3` e `3:4` para o que vai numa página. A imagem sai sempre com
  1600 px no lado maior; o card só a escala, e um clique a abre em tela cheia.
  Mudar a proporção RECOMPUTA o gráfico — é o único param de aparência que
  muda mesmo o desenho, porque o ggplot recoloca a legenda e remede os rótulos.
  Arrastar a alça do card, não: aquilo é tamanho, não proporção.
- **Tema** — `padrão` segue o tema padrão do projeto; os temas (fundo, cores,
  fonte, paleta) são do projeto, e não do gráfico. Sem temas próprios valem os
  embutidos, com `escuro` como padrão; `claro` e `clássico` servem bem ao que
  sai no relatório. O tema dos gráficos não segue o claro/escuro do editor, que
  é preferência de cada pessoa: para mudar todos de uma vez, troque o tema
  padrão em ⚙ Configurações. A paleta só entra onde o
  gráfico não escolheu cores por conta própria.
- **Título**, **Rótulo do X**, **Rótulo do Y** — em branco, o gráfico usa o
  nome da coluna, que costuma ser a legenda certa.
- **Legenda** — `nenhuma` quando a cor já está explicada no título.
