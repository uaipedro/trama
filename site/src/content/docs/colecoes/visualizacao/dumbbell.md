---
title: Halteres
description: Mostra dois ou mais valores por categoria e liga os extremos com um segmento.
section: colecoes
collection: visualizacao
node: view/dumbbell
category: comparacao
related: [view/paired, view/dotplot, data/pivot_longer]
---

## O que o bloco faz

`view/dumbbell` mostra dois ou mais valores por categoria e liga os extremos com um segmento.

## Quando usar

Use para comparar condições dentro de categorias quando os valores estão em formato longo.

## Configuração

Categoria, Condição e Valor numérico são obrigatórios. Valores repetidos no par são somados; são necessárias ao menos duas condições. Ordenar pela diferença (padrão TRUE) ordena pela última menos a primeira condição.

## Exemplo

```r
library(trama.view)
dados <- data.frame(regiao = rep(c("Norte", "Sul", "Leste"), each = 2),
                    ano = rep(c(2025, 2026), 3), receita = c(12, 15, 9, 8, 11, 14))
tr_dumbbell(dados, x = "regiao", cor = "ano", y = "receita")
```

## Como interpretar

Cada linha horizontal corresponde a uma categoria; os pontos marcam os valores das condições e o segmento liga o menor ao maior. A direção e o comprimento mostram a diferença entre as condições extremas. Para interpretar qual ponto corresponde a cada condição e calcular a diferença assinada, leia a legenda e a ordem dos níveis de Condição.
