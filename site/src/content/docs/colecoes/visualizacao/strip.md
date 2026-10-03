---
title: Faixa de pontos
description: Mostra cada observação em uma faixa por grupo, com deslocamento horizontal para reduzir sobreposição.
section: colecoes
collection: visualizacao
node: view/strip
category: distribuicao
related: [view/boxplot, view/violin, view/paired]
---

## O que o bloco faz

`view/strip` mostra cada observação em uma faixa por grupo, com deslocamento horizontal para reduzir sobreposição.

## Quando usar

Use quando os grupos têm poucas observações e os valores individuais importam.

## Configuração

Medida é obrigatória. Grupo vazio forma uma faixa. Estilo aceita colmeia ou espalhado; Traço de resumo aceita mediana, média ou nenhum. Cor por colore sem dividir faixas. Eixo em log requer valores positivos.

## Exemplo

```r
library(trama.view)
dados <- data.frame(tratamento = rep(c("A", "B"), each = 4),
                    altura = c(8, 9, 10, 11, 11, 12, 13, 15))
tr_strip(dados, y = "altura", x = "tratamento", estilo = "colmeia", resumo = "mediana")
```

## Como interpretar

Cada ponto é uma observação; sua altura continua sendo o valor medido. O deslocamento horizontal apenas separa pontos próximos. Na colmeia, a largura local mostra onde valores se repetem; o traço opcional marca a média ou mediana calculada no grupo.
