---
title: Gráfico de defasagens
description: "A série contra ela mesma k períodos antes, um painel por defasagem."
section: colecoes
collection: series-temporais
node: series/lag_plot
category: Ver
order: 2
related: [series/acf, series/lag]
---

## O que o bloco faz

Um disperso por defasagem: no painel k, cada ponto é o valor de um período
contra o valor de k períodos antes. É o correlograma sem o resumo — cada painel
é a nuvem cuja correlação é uma barra do `series/acf`.

Mostra o que o número esconde: relação curva, um ponto sozinho puxando a
correlação, dois regimes. Pontos colados na diagonal tracejada são
autocorrelação forte.

Numa série sazonal os pontos saem coloridos pela estação, e o painel do ciclo
(12 na mensal) é o que fica mais colado na diagonal — janeiro perto de janeiro.

## Quando usar

Investigue a relação entre os valores e suas versões defasadas. Padrões diagonais, curvas ou agrupamentos ajudam a revelar dependência serial e não linearidade.

## Configuração

- **Defasagens** — quantos painéis, até 16. 0 é o ciclo (até 12) numa série
  sazonal, e 4 numa sem ciclo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("lags", "series/lag_plot", from = "pax")
```

## Como interpretar

Um gráfico (`view/plot`).

## Veja também

`series/acf` para o resumo numérico; `series/lag` para montar a série
defasada numa tabela.

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
