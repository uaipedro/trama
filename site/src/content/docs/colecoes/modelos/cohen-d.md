---
title: Tamanho de efeito (dois grupos)
description: "d de Cohen e g de Hedges entre dois grupos, com intervalo de confiança."
section: colecoes
collection: modelos
node: models/cohen_d
category: testes
related: [models/t_test, models/effect_size]
---

## O que o bloco faz

`models/cohen_d` mede a diferença entre as médias de dois grupos em desvios padrão: o d de Cohen (com o desvio padrão combinado) e o g de Hedges (o d corrigido do viés em amostras pequenas), cada um com intervalo de confiança. A saída é uma tabela de duas linhas.

## Quando usar

Ao lado de um [t para duas amostras](/trama/colecoes/modelos/t-test/): o t diz se as médias diferem, o d diz de quanto, numa escala que se compara entre estudos. Com menos de 20 observações por grupo, prefira o g.

## Configuração

Resposta é a coluna numérica; Grupo, a coluna com dois níveis (o primeiro, na ordem dos níveis, é o do numerador); Confiança, o nível do intervalo (padrão 0,95). O intervalo é o normal de Hedges & Olkin, aproximado.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "ToothGrowth") |>
  tr_add("d", "models/cohen_d", resposta = "len", grupo = "supp", from = "dados")
```

No ToothGrowth, o suco de laranja (OJ) fica meio desvio padrão acima do ácido ascórbico (VC): d = 0,49, com intervalo de 95% de −0,02 a 1,01 — efeito médio, estimado com pouca precisão.

## Como interpretar

Sinal positivo: o primeiro grupo tem a maior média. As referências de Cohen (0,2 pequeno, 0,5 médio, 0,8 grande) são genéricas; o intervalo diz o quanto a estimativa é precisa, e um intervalo que cruza o zero equivale a um t não significativo.
