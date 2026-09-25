---
title: Série no tempo
description: "Desenha a série ao longo do tempo, e uma segunda série sobre ela."
section: colecoes
collection: series-temporais
node: series/plot
category: Ver
order: 2
related: [series/seasonal_plot, series/plot_decomposition, series/detrend]
---

## O que o bloco faz

A série como linha no tempo, que é o primeiro gráfico de qualquer análise:
tendência, sazonalidade, quebras, outliers e mudança de variância se veem
aqui antes de qualquer teste.

O card de toda série já mostra este gráfico. O nó existe para o que o card não
escolhe: proporção, tema claro para o relatório, título — e para sair como
`view/plot`, igual aos gráficos da coleção `view`.

Faltante interrompe a linha: o buraco aparece como buraco.

### Série sobreposta

A entrada opcional **sobreposta** desenha uma segunda série no mesmo eixo, com
legenda: a tendência estimada sobre a série original, antes de subtraí-la; a
média móvel; o ajuste de outro modelo. Ligue a série original em **serie** e,
por exemplo, `series/component` com `tendencia` (de uma `series/regression`
de grau 1, para a tendência linear) em **sobreposta**.

As duas precisam ter a mesma frequência; leve uma à outra com
`series/aggregate`. Períodos diferentes são aceitos: o eixo cobre a união, e
cada linha ocupa o seu trecho.

## Quando usar

Inspecione a evolução da série no tempo para reconhecer tendência, sazonalidade, mudanças de nível, dispersão e observações atípicas.

## Configuração

- **Marcar pontos** — desenha cada observação sobre a linha. Útil em série
  curta, e para ver onde estão os faltantes.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("g", "series/plot", pontos = TRUE, titulo = "Vazão anual do Nilo",
         tema = "claro", from = "nilo")

# A tendência linear sobre a série, antes de tirá-la:
tr_flow(reg) |>
  tr_add("ap", "series/example", dataset = "AirPassengers") |>
  tr_add("reg", "series/regression", grau = 1L, from = "ap") |>
  tr_add("tend", "series/component", componente = "tendencia", from = "reg") |>
  tr_add("g", "series/plot", from = c("ap", "tend"))
```

## Como interpretar

Um gráfico (`view/plot`). No console, um ggplot comum, somável.

## Veja também

`series/detrend` para tirar a tendência depois de vê-la; `view/line` para
várias séries numa tabela; `series/seasonal_plot` para o
padrão sazonal; `series/plot_decomposition` para os componentes.

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
