---
title: "Indicador"
description: "Transforma a variável num indicador 0/1 num valor de corte, para a krigagem estimar probabilidade em vez do valor."
section: colecoes
collection: espacial
node: spatial/indicator
category: "Preparar"
related: [spatial/variogram, spatial/explore]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Troca a variável por um **indicador**: 1 onde ela satisfaz a condição, 0 onde
não. Daí o fluxo segue igual — variograma, ajuste, krigagem —, e o que a
krigagem estima passa a ser a **probabilidade** de a condição valer em cada
célula, não o valor da variável.

Serve à pergunta que o mapa do predito não responde: não "quanto", mas "qual a
chance de passar deste limite". Teor acima do crítico, rendimento abaixo do que
paga a lavoura, contaminante acima da norma.

**Por que isto é um bloco, e não uma opção da krigagem.** A krigagem indicadora
é a krigagem ordinária de uma variável 0/1, e o que a torna indicadora é o
variograma ser o **do indicador**. Transformar aqui, antes do variograma,
garante que o modelo ajustado descreva o indicador. Se o corte fosse uma opção
do bloco de krigagem, ele receberia um modelo ajustado à variável contínua e
krigaria o indicador com ele — resultado errado, sem erro nenhum na tela.

O predito é recortado em **[0, 1]**: a krigagem é um interpolador linear e sai
desse intervalo de verdade, sobretudo longe dos pontos. A nota do card diz
quantas células precisaram do recorte, porque muitas delas são sinal de modelo
ruim para a pergunta.

## Parâmetros

- **Corte** — o valor que separa, na unidade da variável original. Precisa cair
  dentro do intervalo observado: fora dele o indicador sai constante, e o bloco
  recusa.
- **Sentido** — `<=` estima a probabilidade de **não exceder** o corte; `>`, a
  de exceder. Os dois são complementares e somam 1.

## Valor

Um objeto espacial (`spatial/points`) cuja variável é o indicador, marcado para
que a krigagem e o mapa adiante o leiam como probabilidade.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("pontos", "spatial/example", dataset = "milho_pr") |>
  tr_add("ind", "spatial/indicator", corte = 4000, from = "pontos") |>
  tr_add("v", "spatial/variogram", from = "ind") |>
  tr_add("m", "spatial/variogram_fit", from = "v") |>
  tr_add("k", "spatial/kriging", from = c("ind", "m")) |>
  tr_add("mapa", "spatial/map", from = "k")
```

## Veja também

`spatial/variogram`, que daqui mede a dependência do indicador;
`spatial/explore` para ver a distribuição antes de escolher o corte.

