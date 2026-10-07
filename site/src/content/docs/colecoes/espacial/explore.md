---
title: "Explorar"
description: "Quatro vistas da variável: postplot por quartil na borda, contra cada coordenada e a distribuição."
section: colecoes
collection: espacial
node: spatial/explore
category: "Explorar"
related: [spatial/variogram]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

O que olhar antes de medir dependência espacial. O painel completo reúne quatro
vistas: o **mapa dos pontos** pintados por quartil da variável, dentro da borda;
a **variável contra a coordenada X** e **contra a Y**; e a **distribuição**.

Os gráficos contra as coordenadas são onde tendência de larga escala aparece:
uma nuvem que sobe ou desce de um lado a outro do domínio é tendência, e o
variograma não a distingue de dependência espacial. Se ela está ali, remova-a
no bloco `spatial/variogram` (param **Tendência removida**) antes de ajustar.

No mapa o quartil vai em **tamanho e cor ao mesmo tempo**, para que o gráfico
leia em preto e branco e para quem não distingue as cores. Os eixos do mapa
têm sempre a mesma escala (`coord_equal`): proporção diferente entre X e Y
distorceria a geometria, que é o que o mapa existe para mostrar. Uma variável
sem variação sai com uma só classe, e um empate de quartis funde classes.

## Parâmetros

- **Vista** — `completo` (padrão), ou só um: `mapa`, `x`, `y`, `histograma`.
  Os cosméticos abaixo valem para o painel inteiro.

## Valor

Um gráfico (`view/plot`). No painel completo é uma composição de quatro
gráficos (um `patchwork`, que continua sendo um ggplot).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_pr") |>
  tr_add("e", "spatial/explore", vista = "completo", from = "p")
```

## Veja também

`spatial/variogram` para medir a dependência, com a tendência removida se o
painel a mostrou.


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

