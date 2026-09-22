---
title: Violino
description: Mostra a densidade espelhada de uma medida em cada grupo.
section: colecoes
collection: visualizacao
node: view/violin
category: distribuicao
related: [view/boxplot, view/strip, view/means]
---

## O que o bloco faz

`view/violin` mostra a densidade espelhada de uma medida em cada grupo.

## Quando usar

Use para comparar forma e possíveis concentrações múltiplas entre grupos.

## Configuração

Medida é obrigatória e numérica; Grupo vazio produz um violino da amostra. Preencher por subdivide cada grupo. Caixa por dentro (padrão TRUE) acrescenta mediana e quartis; Mostrar observações (padrão FALSE) sobrepõe linhas. Eixo em log exige valores positivos.

## Exemplo

```r
library(trama.view)
dados <- data.frame(regiao = rep(c("Norte", "Sul"), each = 6),
                    valor = c(5, 6, 6, 7, 8, 9, 8, 9, 10, 11, 12, 15))
tr_violin(dados, y = "valor", x = "regiao", caixa = TRUE, pontos = TRUE)
```

## Como interpretar

A largura do violino em cada altura representa densidade estimada, não quantidade de observações; grupos pequenos podem parecer tão largos quanto grupos grandes. A caixa interna mostra mediana e quartis. Pontos observados ajudam a conferir o tamanho da amostra e se as barrigas da densidade são sustentadas pelos dados.
