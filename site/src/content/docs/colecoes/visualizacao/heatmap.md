---
title: Mapa de calor
description: Resume cada par de categorias numa célula colorida pela contagem ou soma.
section: colecoes
collection: visualizacao
node: view/heatmap
category: relacao
related: [view/bars, view/bin2d, data/group_summarise]
---

## O que o bloco faz

`view/heatmap` resume cada par de categorias numa célula colorida pela contagem ou soma.

## Quando usar

Use para localizar padrões entre duas categorias. Sem Valor, conta linhas; com Valor, soma a coluna numérica por par.

## Configuração

Colunas e Linhas são obrigatórias; Valor é opcional e numérico. Escrever valores anota cada célula. Chaves faltantes são excluídas; pares sem linhas ficam sem célula. Para média ou outra estatística, agregue antes por ambas as chaves.

## Exemplo

```r
library(trama.view)
dados <- data.frame(mes = rep(c("jan", "fev"), each = 3),
                    regiao = rep(c("Norte", "Sul", "Leste"), 2),
                    receita = c(12, 8, 10, 15, 7, 13))
tr_heatmap(dados, x = "mes", y = "regiao", valor = "receita", rotulos = TRUE)
```

## Como interpretar

Cada célula representa um par de valores de Colunas e Linhas. A cor codifica a contagem ou soma e, com Escrever valores ligado, o rótulo mostra o número agregado. Célula ausente significa que o par não aparece nos dados; não equivale a uma célula observada cujo valor é zero.
