---
title: Gráfico da decomposição
description: "Os quatro componentes da decomposição, empilhados."
section: colecoes
collection: series-temporais
node: series/plot_decomposition
category: Ver
order: 2
related: [series/stl, series/decompose, series/component]
---

## O que o bloco faz

Desenha a série e os três componentes — tendência, sazonal, resto — em
painéis empilhados, com o tempo em comum.

**Cada painel tem a própria escala**, e é o que se tem de ler com cuidado: um
sazonal de ±40 e um resto de ±5 saem com a MESMA altura. O tamanho de cada
componente está nos números do eixo, não na altura do desenho. Um resto com
estrutura visível (ondas, degraus) quer dizer que a decomposição deixou algo
para trás.

O card da decomposição já mostra este gráfico; o nó existe para escolher
proporção, tema e título. A proporção padrão é 4:3 porque quatro painéis em
16:9 viram fitas.

## Quando usar

Compare no mesmo painel a série observada e os componentes de uma decomposição. Use-o para avaliar se a separação representa a estrutura temporal de forma plausível.

## Configuração

Só os de aparência, abaixo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("co2", "series/example", dataset = "co2") |>
  tr_add("stl", "series/stl", from = "co2") |>
  tr_add("g", "series/plot_decomposition", tema = "claro", from = "stl")
```

## Como interpretar

Um gráfico (`view/plot`).

## Veja também

`series/stl` e `series/decompose` para a decomposição; `series/component` para
um componente só.

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
