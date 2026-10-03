---
title: Anotação
description: Escreve um texto numa coordenada do gráfico, com seta opcional.
section: colecoes
collection: visualizacao
node: view/annotate
category: camadas
related: [view/labels, view/reference, view/points]
---

## O que o bloco faz

`view/annotate` recebe um gráfico e escreve um texto no ponto (X, Y), nas unidades dos eixos. Com **Seta até X** e **Seta até Y**, desenha uma seta do texto até esse ponto.

## Quando usar

Use para apontar o que o leitor deve notar: um outlier, o início de um tratamento, uma mudança de regime. Para escrever o nome de todos os pontos, `view/labels` é o bloco certo.

## Configuração

**X** e **Y** aceitam vírgula decimal e valem para eixos numéricos. **Texto** é obrigatório. A seta precisa dos dois campos preenchidos, ou de nenhum: preencher só um dos dois é erro. Com painéis, a anotação aparece em todos.

## Exemplo

```r
library(trama.view)
p <- tr_points(mtcars, x = "wt", y = "mpg")
tr_annotate(p, x = "4", y = "30", texto = "carros leves", seta_x = "2", seta_y = "32")
```

## Como interpretar

A anotação não vem dos dados: se o dado mudar, confira se o texto ainda aponta para o lugar certo.
