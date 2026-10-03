---
title: Grade de densidade
description: Conta observações em células de uma grade definida por duas medidas.
section: colecoes
collection: visualizacao
node: view/bin2d
category: relacao
related: [view/points, view/heatmap, data/filter]
---

## O que o bloco faz

`view/bin2d` conta observações em células de uma grade definida por duas medidas.

## Quando usar

Use quando a sobreposição torna um disperso uma mancha. Cada célula codifica a contagem de linhas.

## Configuração

Eixo X e Eixo Y são obrigatórios e numéricos. Classes define o número de divisões em cada eixo (padrão 40; interface aceita 5–200). Eixo em log aceita nenhum, X, Y ou ambos e requer valores positivos; Painéis por separa grupos.

## Exemplo

```r
library(trama.view)
dados <- data.frame(idade = c(20, 21, 21, 35, 36, 36), renda = c(30, 31, 33, 55, 57, 58))
tr_bin2d(dados, x = "idade", y = "renda", classes = 10L)
```

## Como interpretar

Cada célula representa a contagem de linhas cujos valores de X e Y caem naquele retângulo; a cor codifica essa contagem. O fundo sem célula indica ausência de linhas naquela região. Classes menores agrupam mais observações por célula e escondem detalhe; classes maiores destacam variação local, mas podem deixar muitas células vazias.
