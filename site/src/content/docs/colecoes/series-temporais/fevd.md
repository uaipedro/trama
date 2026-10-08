---
title: "Decomposição da variância"
description: "Quanto da variância do erro de previsão de cada série vem de cada choque, por horizonte."
section: colecoes
collection: series-temporais
node: series/fevd
category: "Ver"
related: [series/irf, series/var]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

De quanto é o erro de previsão de cada série em cada horizonte, e de qual
choque ele vem. No horizonte 1, a variância de uma série vem só dos choques
dela e dos das anteriores na ordem; com o tempo, a fatia das outras cresce.

Cada barra soma 1 (100%): é a fração da variância explicada por cada choque.
Uma faixa por série.

A separação dos choques é a de Cholesky, e a fração que cabe a cada série
DEPENDE DA ORDEM delas no modelo. Uma série posta por último recebe o que sobra
depois das anteriores.

## Parâmetros

- **Horizonte** — até onde prever o erro (1 a 100 períodos).

## Valor

Um gráfico (`view/plot`). No console, um ggplot comum, somável.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("fevd", "series/fevd", horizonte = 12L, from = "v")
```

## Veja também

`series/irf` para as respostas que geram essa variância; `series/var`.

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

