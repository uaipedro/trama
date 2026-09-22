---
title: Gráfico sazonal
description: "Um traço por ano, percorrendo o ciclo: o formato da sazonalidade."
section: colecoes
collection: series-temporais
node: series/seasonal_plot
category: Ver
order: 2
related: [series/subseries, series/stl]
---

## O que o bloco faz

Cada ano vira uma linha, e o eixo horizontal é o ciclo — jan a dez numa série
mensal, T1 a T4 numa trimestral. Responde duas perguntas de uma vez:

- qual é o FORMATO da sazonalidade — o pico é em julho? há dois picos?
- ele MUDA com o tempo — os anos recentes (cor mais clara) repetem a forma dos
  antigos, ou o pico migrou?

As linhas empilhadas de baixo para cima são a tendência; o formato de cada uma
é a sazonalidade. Se as oscilações crescem com o nível, a sazonalidade é
multiplicativa.

Pede série sazonal (frequência maior que 1).

## Quando usar

Compare anos ao longo das posições do ciclo para observar como a forma sazonal se repete ou muda com o tempo.

## Configuração

Só os de aparência, abaixo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("saz", "series/seasonal_plot", from = "pax")
```

## Como interpretar

Um gráfico (`view/plot`).

## Veja também

`series/subseries` para como cada estação evoluiu; `series/stl` para medir a
sazonalidade.

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
