---
title: Médias com barras
description: Calcula a média por grupo e mostra uma barra de incerteza ou dispersão.
section: colecoes
collection: visualizacao
node: view/means
category: comparacao
related: [view/boxplot, view/bars, view/violin]
---

## O que o bloco faz

`view/means` calcula a média por grupo e mostra uma barra de incerteza ou dispersão.

## Quando usar

Use quando a pergunta é sobre médias e a medida de incerteza precisa estar explícita.

## Configuração

Grupo e Medida são obrigatórios; Medida precisa ser numérica. Separar por e Painéis por subdividem o resumo. Barra aceita IC (padrão), erro padrão ou desvio padrão; Confiança (IC) dá o nível do IC (padrão 0,95) e aparece no rótulo do eixo. Grupo com uma observação tem ponto, sem barra.

## Exemplo

```r
library(trama.view)
dados <- data.frame(regiao = rep(c("Norte", "Sul"), each = 4),
                    valor = c(8, 9, 10, 11, 12, 13, 14, 15))
tr_means(dados, x = "regiao", y = "valor", barra = "IC", confianca = 0.95)
```

## Como interpretar

Cada ponto é a média das linhas do grupo e cada barra mostra a opção escolhida. IC representa incerteza da média, no nível escolhido; erro padrão é uma medida de incerteza menor; desvio padrão descreve espalhamento das observações. Uma observação produz ponto sem barra, pois não permite estimar desvio.
