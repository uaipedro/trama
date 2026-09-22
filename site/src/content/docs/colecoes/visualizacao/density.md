---
title: Densidade
description: Estima uma curva suavizada para mostrar a forma de uma medida numérica.
section: colecoes
collection: visualizacao
node: view/density
category: distribuicao
related: [view/histogram, view/ecdf, view/violin]
---

## O que o bloco faz

`view/density` estima uma curva suavizada para mostrar a forma de uma medida numérica.

## Quando usar

Use para comparar formas de distribuição sem definir classes.

## Configuração

Medida é obrigatória. Separar por colore uma curva por grupo. Suavidade multiplica a largura de banda automática (padrão 1): valores maiores alisam mais. Eixo em log requer valores positivos; Painéis por separa grupos.

## Exemplo

```r
library(trama.view)
dados <- data.frame(valor = c(8, 9, 10, 11, 12, 13, 15, 17),
                    regiao = rep(c("Norte", "Sul"), each = 4))
tr_density(dados, x = "valor", cor = "regiao", suavidade = 1)
```

## Como interpretar

O eixo horizontal contém os valores; a altura estima densidade, não contagem. A área total sob cada curva é um, por isso a altura compara concentração relativa, não o tamanho dos grupos. Suavidade maior reduz picos e vales; confira o histograma ou a acumulada se a forma depender muito desse ajuste.
