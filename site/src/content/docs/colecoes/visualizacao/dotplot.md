---
title: Pontos ordenados
description: Resume categorias em pontos num ranking horizontal.
section: colecoes
collection: visualizacao
node: view/dotplot
category: comparacao
related: [view/bars, view/dumbbell, data/group_summarise]
---

## O que o bloco faz

`view/dotplot` resume categorias em pontos num ranking horizontal.

## Quando usar

Use para rankings com muitas categorias nomeadas.

## Configuração

Categoria é obrigatória. Valor vazio conta linhas; preenchido soma valores. Cor por cria um ponto por subgrupo. Estilo aceita pontos ou pirulito; Ordenar pelo valor vem ligado. Para médias, calcule antes.

## Exemplo

```r
library(trama.view)
dados <- data.frame(municipio = c("A", "B", "C", "A", "B", "C"),
                    producao = c(4, 7, 5, 3, 2, 6))
tr_dotplot(dados, x = "municipio", y = "producao", ordenar = TRUE)
```

## Como interpretar

Cada ponto resume uma categoria: sem Valor, a contagem de linhas; com Valor, a soma. O eixo vertical lista as categorias e o horizontal mostra o resumo. Ordenar pelo valor facilita localizar extremos; pontos de subgrupos na mesma linha comparam subgrupos, e o intervalo entre eles não é uma barra de incerteza.
