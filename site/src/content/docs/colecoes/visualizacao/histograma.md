---
title: Histograma
description: Agrupe valores numéricos em faixas para examinar a distribuição de uma medida.
section: colecoes
collection: visualizacao
node: view/histogram
category: distribuicao
order: 3
related: [view/density, view/ecdf, view/boxplot]
---

## O que o bloco faz

`view/histogram` divide uma medida numérica em faixas e mostra a contagem de observações em cada faixa. Cada barra representa uma faixa, não uma categoria agregada.

## Quando usar

Use para examinar concentração, assimetria, lacunas e possíveis valores extremos numa medida. Para comparar grupos, preencha **Separar por**; para comparar a forma sem contagens por faixa, considere `view/density` ou `view/ecdf`.

## Configuração

**Eixo X** é obrigatório. **Classes** define o número de faixas (padrão 30). Poucas faixas escondem detalhes; muitas podem deixar contagens pequenas e instáveis. **Separar por** colore distribuições por grupo. **Posição** aceita `empilhar` ou `sobrepor`; sobrepor usa transparência. **Eixo em log** requer valores positivos e constrói faixas de largura igual na escala transformada. **Painéis por** separa grupos em painéis de escala comum.

## Exemplo

```r
library(trama.view)
dados <- data.frame(valor = c(4, 5, 5, 6, 7, 8, 8, 9, 11, 12),
                    grupo = rep(c("A", "B"), each = 5))
tr_histogram(dados, x = "valor", classes = 5L, cor = "grupo", posicao = "sobrepor")
```

## Como interpretar

A altura de cada barra é a contagem de linhas dentro da faixa. Alterar Classes muda os limites e pode mudar a forma aparente. Nuvens de grupos sobrepostas podem ocultar diferenças; painéis ou a curva acumulada tornam a comparação mais legível.

## Veja também

`view/density` suaviza a forma; `view/ecdf` mostra proporções acumuladas sem escolher classes; `view/boxplot` compara resumos entre grupos.
