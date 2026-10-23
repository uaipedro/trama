---
title: "Mapa da superfície"
description: "Desenha a superfície krigada, ou o erro-padrão dela, sobre a borda e os pontos."
section: colecoes
collection: espacial
node: spatial/map
category: "Predizer"
related: [spatial/kriging]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Quando a superfície vem de um indicador (`spatial/indicator`), o predito **é
probabilidade**, e o mapa diz isso: a legenda traz "Probabilidade de ..." com o
corte, em vez do nome da variável. Sem esse cuidado o mapa apresentaria
probabilidade com a cara de rendimento ou de teor.

Desenha a superfície da krigagem. **O mapa do erro-padrão é o par honesto do
mapa do predito**: o predito é liso e convincente em qualquer lugar, e só o
erro-padrão mostra onde ele vale pouco, longe dos pontos e perto das bordas.
Por isso os dois saem do mesmo bloco, escolhidos por **Mostrar**: o mapa honesto
nunca é mais difícil de pedir que o bonito. Leia um com o outro, e não publique
o predito sozinho.

Os eixos têm sempre a mesma escala (`coord_equal`). Célula sem predição (nenhum
ponto dentro do **Raio** da krigagem) sai em cinza, e não em branco, para que não
se confunda com a borda.

## Parâmetros

- **Mostrar** — `predito` (padrão) ou `erro-padrao`.
- **Isolinhas** — curvas de nível sobre a superfície.
- **Pontos amostrais** — as localizações medidas, por cima; mostram de onde
  vem a informação.

## Valor

Um gráfico (`view/plot`): a grade em `geom_raster`, a borda do domínio e,
quando ligados, as isolinhas e os pontos.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_se") |>
  tr_add("v", "spatial/variogram", from = "p") |>
  tr_add("m", "spatial/variogram_fit", from = "v") |>
  tr_add("k", "spatial/kriging", from = c("p", "m")) |>
  tr_add("mapa", "spatial/map", mostrar = "erro-padrao", from = "k")
```

## Veja também

`spatial/kriging`, que produz a superfície.


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

