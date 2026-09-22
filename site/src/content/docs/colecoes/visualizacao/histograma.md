---
title: Histograma
description: Agrupe valores numéricos em faixas para examinar a distribuição de uma medida.
section: colecoes
collection: visualizacao
node: view/histogram
category: distribuicao
order: 3
related: [data/summary, view/points]
---

## Finalidade

`view/histogram` agrupa valores numéricos em faixas e mostra quantas observações
caem em cada uma delas. Ele é usado para examinar concentração, assimetria,
lacunas e valores extremos.

## Configuração

**Eixo X** recebe a medida numérica. **Classes** controla a quantidade de
faixas: poucas classes suavizam a forma; muitas classes podem produzir
contagens instáveis em bases pequenas.

> **Antes de continuar**
>
> A altura de uma barra é uma contagem de observações na faixa, não uma medida
> calculada por categoria. Para comparar médias por grupo, use um resumo antes
> e escolha um gráfico de comparação.

## Condições de leitura

Trocar a quantidade de classes altera as faixas e pode alterar a forma aparente
da distribuição. A interpretação deve considerar essa escolha e a quantidade
de observações disponível.
