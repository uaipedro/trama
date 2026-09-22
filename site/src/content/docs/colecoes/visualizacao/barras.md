---
title: Barras
description: Compare contagens ou somas entre categorias com comprimentos a partir do zero.
section: colecoes
collection: visualizacao
node: view/bars
category: comparacao
related: [view/means, view/dotplot, view/heatmap, data/group_summarise]
---

## O que o bloco faz

`view/bars` desenha uma barra por categoria. Sem **Altura**, cada barra conta linhas; com **Altura**, soma os valores dessa coluna dentro de cada categoria.

## Quando usar

Use para comparar contagens ou totais entre categorias. Para médias, calcule uma média por grupo antes com `data/group_summarise`; para preservar a distribuição de cada grupo, use `view/boxplot`.

## Configuração

**Categoria** é obrigatória. **Altura** é opcional e numérica; vazia significa contagem, preenchida significa soma. **Subgrupo** divide cada categoria por uma segunda variável. **Posição** aceita `empilhar`, `lado a lado` ou `proporção`; proporção compara parcelas dentro de cada categoria e remove a comparação de totais. **Ordenar pela altura** classifica pelas somas ou contagens. **Deitar** põe categorias no eixo vertical. **Escrever valores** anota barras ou fatias. **Painéis por** separa grupos em painéis.

## Exemplo

```r
library(trama.view)
dados <- data.frame(regiao = c("Norte", "Norte", "Sul", "Sul", "Sul"),
                    receita = c(12, 8, 7, 9, 4), produto = c("A", "B", "A", "B", "A"))
tr_bars(dados, x = "regiao", y = "receita", cor = "produto",
        posicao = "lado a lado", ordenar = TRUE, deitar = TRUE, rotulos = TRUE)
```

## Como interpretar

A altura total compara soma por categoria; com subgrupos empilhados, as fatias mostram composição e apenas a inferior parte do zero. `lado a lado` compara subgrupos a partir de uma base comum. `proporção` mostra participação percentual por categoria, de modo que categorias de tamanhos diferentes passam a ter a mesma altura.
