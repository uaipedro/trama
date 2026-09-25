---
title: Painel
description: Junta gráficos numa figura com painéis etiquetados A, B, C e um tema só.
section: colecoes
collection: visualizacao
node: view/combine
category: figura
related: [view/points, view/boxplot, view/bars]
---

## O que o bloco faz

`view/combine` junta dois ou mais gráficos numa figura só, em linha ou em grade, com uma etiqueta em cada painel (A, B, C). O tema escolhido no painel vale para todos os gráficos, qualquer que fosse o tema de cada um.

## Quando usar

Use para montar a figura com vários painéis de um artigo ou de uma tese dentro do fluxo: quando o dado muda, a figura é refeita sem passar por editor de imagem.

## Configuração

A ordem dos painéis é a ordem em que os gráficos foram ligados à entrada: o primeiro ligado é o A. Colunas define quantos painéis por linha; 0 deixa uma grade quase quadrada. Etiquetas aceita `A, B, C`, `a, b, c`, `1, 2, 3` ou `nenhuma`. Legenda comum junta legendas iguais numa só. O título vira o título da figura inteira, e rótulos de eixo preenchidos valem para todos os painéis.

## Exemplo

```r
library(trama.view)
a <- tr_points(mtcars, x = "wt", y = "mpg", titulo = "Consumo e peso")
b <- tr_boxplot(mtcars, x = "cyl", y = "mpg", titulo = "Consumo por cilindros")
tr_combine(list(a, b), colunas = 2, tema = "clássico", aspecto = "2:1")
```

## Como interpretar

Cada painel mantém os próprios eixos e escalas; só a aparência é comum. Compare painéis pela forma e pela posição relativa, e confira as escalas antes de comparar alturas entre eles.
