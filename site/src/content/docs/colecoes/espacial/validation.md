---
title: "Validação cruzada"
description: "Mede o modelo predizendo cada ponto sem ele mesmo: erro médio, RMSE, MSDR e correlação entre observado e predito."
section: colecoes
collection: espacial
node: spatial/validation
category: "Predizer"
related: [spatial/kriging, spatial/variogram_fit]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Um modelo de variograma que descreve bem o variograma empírico ainda pode
krigar mal. A validação cruzada mede isso: tira um ponto de cada vez, prediz o
lugar dele com os outros, e compara com o valor observado.

As quatro medidas, e o que cada uma pega:

- **Erro médio (ME)** — perto de zero é o que se espera; longe de zero indica
  viés sistemático, o mapa inteiro deslocado para cima ou para baixo.
- **RMSE** — o tamanho típico do erro, na unidade da variável. Serve para
  comparar modelos no MESMO conjunto de pontos.
- **MSDR** — a média de (resíduo dividido pelo erro-padrão) ao quadrado. É a
  única que olha o **mapa de erro-padrão**: perto de 1 ele está calibrado; muito
  acima de 1 o mapa é otimista, promete precisão que não tem; muito abaixo é
  pessimista. Como a krigagem entrega sempre dois mapas, essa é a medida que
  diz se o segundo presta.
- **Correlação** entre observado e predito — quanto da variação o modelo
  acompanha.

**Leave-one-out** deixa um ponto de fora por vez: usa o máximo da informação e
é determinístico. **K dobras** deixa um grupo de fora por vez, treinando com
menos pontos; o erro sai maior, e mais perto do que se espera de uma predição
em lugar de verdade não amostrado. A partição em dobras é sorteada, então fixe
a **semente** para o card não mudar a cada execução.

## Parâmetros

- **Método** — `leave-one-out` ou `k dobras`.
- **Dobras** — quantos grupos, no método de dobras. De 2 até o número de
  pontos; acima disso o bloco recusa e diz o máximo.
- **Semente** — fixa o sorteio das dobras.
- **Vizinhos**, **Raio** — a mesma vizinhança da krigagem. Validar com a
  vizinhança que você vai usar no mapa é o que torna a medida comparável.

## Valor

Uma validação (`spatial/validation`): uma linha por ponto, com observado,
predito, variância, resíduo e o resíduo padronizado, mais as quatro métricas.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("pontos", "spatial/example", dataset = "milho_pr") |>
  tr_add("v", "spatial/variogram", from = "pontos") |>
  tr_add("m", "spatial/variogram_fit", from = "v") |>
  tr_add("valid", "spatial/validation", from = c("pontos", "m"))
```

## Veja também

`spatial/kriging` para o mapa; `spatial/variogram_fit` para o modelo que esta
validação julga.

