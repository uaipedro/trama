---
title: Subséries sazonais
description: "Um painel por estação, com os anos em sequência e a média de cada estação."
section: colecoes
collection: series-temporais
node: series/subseries
category: Ver
order: 2
related: [series/seasonal_plot, series/aggregate]
---

## O que o bloco faz

Um painel por estação (mês, trimestre), com os anos em sequência dentro de
cada um e a média daquela estação tracejada. É o complemento do
`series/seasonal_plot`: aquele mostra o formato do ciclo; este mostra como
CADA estação evoluiu.

As médias tracejadas desenham o padrão sazonal médio (a altura de cada painel).
As linhas mostram se uma estação se afastou das outras: um julho que sobe mais
que os outros meses — sazonalidade que muda — é invisível no sazonal e óbvio
aqui.

Pede série sazonal com pelo menos dois ciclos, e frequência até 24 — acima
disso seriam painéis demais; agregue antes com `series/aggregate`.

## Quando usar

Compare, em painéis separados, as observações de cada posição sazonal ao longo dos ciclos. As médias ajudam a identificar diferenças entre estações.

## Configuração

Só os de aparência, abaixo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("gas", "series/example", dataset = "UKgas") |>
  tr_add("sub", "series/subseries", from = "gas")
```

## Como interpretar

Um gráfico (`view/plot`).

## Veja também

`series/seasonal_plot` para o formato do ciclo; `series/aggregate` quando a
frequência é alta demais.

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
