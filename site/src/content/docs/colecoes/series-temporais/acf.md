---
title: Correlograma (ACF)
description: "A autocorrelação da série em cada defasagem, com a banda do ruído branco."
section: colecoes
collection: series-temporais
node: series/acf
category: Ver
order: 2
related: [series/pacf, series/ljung_box, series/lag_plot]
---

## O que o bloco faz

O correlograma: para cada defasagem k, a correlação da série com ela mesma k
períodos antes. É o retrato da memória da série.

### Como ler

- barras que caem DEVAGAR, ainda altas depois de dezenas de defasagens: série
  não estacionária (tendência). Diferencie antes de ler o resto.
- picos nos múltiplos do ciclo (12, 24, 36 numa mensal): sazonalidade. As
  marcas do eixo caem justamente nesses pontos.
- cortar abruptamente depois da defasagem q: assinatura de um MA(q).
- decair aos poucos (exponencial ou senoidal): assinatura de um AR — confira no
  `series/pacf`.

A linha tracejada é a banda de ±1,96/√n: onde fica uma autocorrelação que é
zero, 95% das vezes. As barras FORA dela saem coloridas. Com 36 defasagens,
espere uma ou duas fora por puro acaso (5% de 36 é 1,8) — uma barra isolada e
pequena fora da banda, numa defasagem sem significado, não é estrutura.

O eixo é em DEFASAGENS (1, 2, … 36), e não na unidade de tempo que o `acf()`
do R usa para série sazonal (0,083, 0,167, … 3).

## Quando usar

Examine dependências entre observações em diferentes defasagens. A ACF também ajuda a localizar persistência e ciclos sazonais que ainda aparecem na série ou nos resíduos.

## Configuração

- **Defasagens** — até onde ir. 0 é automático: 10·log10(n), mas nunca menos
  de três ciclos numa série sazonal.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d", "series/diff", tipo = "sazonal", from = "log") |>
  tr_add("acf", "series/acf", defasagens = 36L, from = "d") |>
  tr_add("pacf", "series/pacf", defasagens = 36L, from = "d")
```

## Como interpretar

Um gráfico (`view/plot`). No console, um ggplot com a tabela `defasagem`, `r`,
`fora` em `$data`.

## Veja também

`series/pacf`, que se lê junto; `series/ljung_box` para o teste formal;
`series/lag_plot` para ver a nuvem por trás de cada barra.

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
