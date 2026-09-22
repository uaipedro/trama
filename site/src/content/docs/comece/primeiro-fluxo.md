---
title: Primeiro fluxo
description: Crie uma análise pequena com uma tabela de exemplo, uma inspeção e um gráfico.
section: comece
order: 1
related: [data/read_csv, data/summary, view/points]
---

## Finalidade

Um fluxo organiza operações em uma sequência explícita. Cada bloco recebe um
resultado, produz outro e deixa esse resultado disponível para inspeção.

> **Antes de continuar**
>
> Um bloco não é uma versão “mais simples” de uma função R. Ele representa uma
> função configurada dentro de um fluxo: entradas, parâmetros e saída ficam
> visíveis no mesmo lugar.

## Um primeiro caminho

Abra o Trama com as coleções `trama.data` e `trama.view`. Na paleta, acrescente
os blocos **Dados de exemplo**, **Resumo** e **Disperso**. Ligue a saída de um
bloco à entrada do próximo.

```r
library(trama)

tr_app(tr_project(
  "meu-projeto",
  collections = c("trama.data", "trama.view")
))
```

Escolha `mtcars` em **Dados de exemplo**. Em **Disperso**, use `wt` no eixo X,
`mpg` no eixo Y e `cyl` em **Cor por**. O gráfico mostra uma marca por linha da
tabela.

## Como conferir o caminho

O card de cada bloco mostra o resultado correspondente. O **Resumo** descreve
os tipos e faltantes das colunas; o gráfico usa a tabela que chega pela conexão.
Mudar um parâmetro recalcula apenas os blocos afetados adiante no fluxo.

> **Antes de continuar**
>
> Começar com uma tabela pequena serve para separar duas perguntas: se o fluxo
> está montado como esperado e se os dados reais estão preparados para a mesma
> análise. A primeira pode ser respondida antes de escolher um arquivo.
