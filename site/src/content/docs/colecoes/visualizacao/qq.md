---
title: Quantil-quantil
description: Compara os quantis observados de uma medida com os quantis teóricos normais.
section: colecoes
collection: visualizacao
node: view/qq
category: distribuicao
related: [view/histogram, view/density, view/ecdf]
---

## O que o bloco faz

`view/qq` compara os quantis observados de uma medida com os quantis teóricos normais.

## Quando usar

Use para avaliar a forma e os desvios de normalidade, em especial nos resíduos de um modelo.

## Configuração

Medida é obrigatória. Separar por colore grupos na mesma imagem; Painéis por os separa. A reta de referência passa pelos quartis. Curvatura ou afastamento nas pontas indica como a distribuição difere da normal.

## Exemplo

```r
library(trama.view)
dados <- data.frame(residuo = c(-1.8, -1.1, -0.6, -0.2, 0.1, 0.4, 0.9, 2.0))
tr_qq(dados, y = "residuo")
```

## Como interpretar

Cada ponto compara um quantil observado com o quantil normal correspondente; a reta passa pelos quartis da amostra. Pontos próximos à reta são compatíveis com a forma normal; curvatura indica assimetria e afastamentos nas pontas indicam caudas mais leves ou pesadas. O gráfico mostra forma e tamanho do desvio, não um teste de normalidade.
