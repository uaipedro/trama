---
title: Boxplot
description: Compare mediana, quartis e extremos de uma medida entre grupos.
section: colecoes
collection: visualizacao
node: view/boxplot
category: distribuicao
related: [view/histogram, view/violin, view/means]
---

## O que o bloco faz

`view/boxplot` resume uma medida numérica por grupo. A linha dentro da caixa marca a mediana; a caixa cobre o intervalo do primeiro ao terceiro quartil; os bigodes alcançam valores até 1,5 vezes o intervalo interquartil além da caixa. Valores fora desse alcance aparecem como pontos.

## Quando usar

Use para comparar nível e dispersão em alguns grupos. Para ver a forma completa, inclusive concentrações múltiplas, use `view/violin` ou `view/histogram`; para poucos dados, ative as observações ou use `view/strip`.

## Configuração

**Medida** é obrigatória e numérica. **Grupo** define uma caixa por categoria; em branco, o gráfico resume a amostra inteira. **Preencher por** divide caixas lado a lado dentro de cada grupo. **Mostrar observações** sobrepõe cada linha da tabela; com essa opção, os pontos extremos da caixa são ocultados para não aparecer duas vezes. **Eixo em log** requer valores positivos. **Painéis por** separa grupos em painéis com escala comum.

## Exemplo

```r
library(trama.view)
dados <- data.frame(regiao = rep(c("Norte", "Sul"), each = 6),
                    valor = c(5, 6, 7, 8, 9, 14, 8, 9, 10, 11, 12, 13))
tr_boxplot(dados, y = "valor", x = "regiao", pontos = TRUE)
```

## Como interpretar

Caixa mais alta indica maior intervalo entre os quartis; a posição da linha interna compara medianas. Bigodes não representam necessariamente mínimo e máximo, e pontos além deles são observações segundo a convenção de 1,5 IQR, não resultados de um teste de outlier. Grupos com distribuições de formas diferentes podem ter caixas semelhantes, pois o resumo não mostra multimodalidade.
