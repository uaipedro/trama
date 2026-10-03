---
title: Acumulada
description: Mostra a proporção de observações menor ou igual a cada valor.
section: colecoes
collection: visualizacao
node: view/ecdf
category: distribuicao
related: [view/density, view/histogram, view/qq]
---

## O que o bloco faz

`view/ecdf` mostra a proporção de observações menor ou igual a cada valor.

## Quando usar

Use para comparar distribuições sem escolher largura de classe ou suavidade.

## Configuração

Medida é obrigatória. Separar por cria uma curva por grupo. Eixo em log requer valores positivos; Painéis por cria um painel por grupo. A curva cruza 50% na mediana.

## Exemplo

```r
library(trama.view)
dados <- data.frame(valor = c(4, 5, 7, 8, 6, 9, 10, 12),
                    regiao = rep(c("Norte", "Sul"), each = 4))
tr_ecdf(dados, x = "valor", cor = "regiao")
```

## Como interpretar

Em cada X, a altura é a proporção de observações menores ou iguais àquele valor. A mediana está onde a curva cruza 0,5; trechos íngremes indicam concentração e trechos planos indicam intervalos pouco frequentes. Uma curva deslocada para a direita representa valores geralmente maiores que a curva à esquerda.
