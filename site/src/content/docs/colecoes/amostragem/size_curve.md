---
title: Curva do tamanho
description: Como o tamanho da amostra cresce quando a margem de erro aperta, em cada confiança?
section: colecoes
collection: amostragem
node: sampling/size_curve
related: [sampling/size_mean, sampling/size_proportion]
---

## O que o bloco faz

Desenha o n contra a margem de erro, com os mesmos ajustes do plano ligado
(deff, população, não resposta), uma curva para cada confiança, e marca o ponto
do plano.

A curva é a conversa com quem paga a pesquisa: o n cresce com o QUADRADO da
precisão, e metade da margem custa quatro vezes a amostra. É ela que mostra
onde o próximo ponto de precisão fica caro demais.

## Quando usar

Use para comparar a precisão que diferentes tamanhos de amostra alcançam a partir de um plano de média ou proporção.

## Configuração

Só os de aparência.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("plano", "sampling/size_proportion", erro = 0.04, populacao = 5000) |>
  tr_add("curva", "sampling/size_curve", titulo = "Tamanho e margem", from = "plano")
```

## Como interpretar

A figura relaciona n e margem para cada confiança e marca o plano ligado. Como a relação depende do quadrado da precisão, reduzir a margem pela metade exige aproximadamente quatro vezes o n antes dos demais ajustes. A saída é Gráfico `view/plot` de n pela margem de erro.

## Veja também

`sampling/size_mean`, `sampling/size_proportion`.
