---
title: "Impulso-resposta"
description: "Como cada série responde, ao longo do tempo, a um choque numa das outras."
section: colecoes
collection: series-temporais
node: series/irf
category: "Ver"
related: [series/fevd, series/var, series/join]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

A resposta de cada série a um choque de uma unidade numa das séries, em cada
período depois do choque. É o que o VAR diz sobre a dinâmica conjunta: um
choque no preço se propaga para o consumo, e quanto tempo leva para sumir?

Cada faixa é um par impulso → resposta. A linha zero marca o efeito nulo; a
faixa sombreada é a banda de confiança, por bootstrap dos resíduos.

**Ortogonal (Cholesky)** é o padrão, e o resultado DEPENDE DA ORDEM das séries
no modelo: o choque de uma série atinge a de cima no mesmo período, e a de
baixo só depois. Trocar a ordem muda as respostas. Desligado, o choque é o
próprio erro de cada equação, sem a ortogonalização: como os erros são
correlacionados, a resposta não se atribui a uma só série.

O VECM entra pela mesma forma (`vec2var`), e as respostas são as do nível.

## Parâmetros

- **Impulso** — a série que leva o choque. Vazio: todas.
- **Respostas** — as séries que reagem. Vazio: todas. Separe por vírgula.
- **Horizonte** — quantos períodos depois do choque (1 a 100).
- **Ortogonal (Cholesky)** — separa os choques pela ordem das séries (padrão).
- **Acumulada** — soma as respostas período a período: o efeito total até o
  horizonte, e não o efeito em cada período.
- **Reamostras** — réplicas do bootstrap das bandas. 0 desenha só os pontos.
  O padrão, 100, abre rápido no card; para uma banda estável, use 1000 ou mais.
- **Confiança** — o nível da banda, 0.95 por padrão.

A semente é a do nó: a mesma entrada dá as mesmas bandas.

## Valor

Um gráfico (`view/plot`). No console, um ggplot comum, somável.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("irf", "series/irf", impulso = "cac", horizonte = 12L, reamostras = 100L, from = "v")
```

## Veja também

`series/fevd` para a parte da variância explicada por cada choque; `series/var`
para o ajuste; `series/join` para montar as séries.

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

