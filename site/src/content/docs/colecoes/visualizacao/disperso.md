---
title: Disperso
description: Mostre a relação entre duas medidas com uma marca por linha da tabela.
section: colecoes
collection: visualizacao
node: view/points
category: relacao
order: 2
related: [data/summary, data/group_summarise, view/histogram]
---

## Finalidade

`view/points` mostra a relação entre duas medidas. Cada linha da tabela produz
uma marca; cor, forma e tamanho podem distinguir grupos ou outra medida.

## Configuração

Escolha uma coluna para **Eixo X** e outra para **Eixo Y**. **Cor por** separa
grupos sem alterar o número de marcas. Uma linha de tendência pode ser adicionada
quando a relação precisa ser resumida por um modelo simples.

```r
tr_flow(reg) |>
  tr_add("nuvem", "view/points", x = "idade", y = "renda",
         cor = "regiao", tendencia = "linear", from = "dados")
```

> **Antes de continuar**
>
> Um ponto representa uma linha, não necessariamente uma pessoa. Ele pode
> representar município, experimento, medição ou qualquer unidade que uma linha
> da tabela descreva. Essa unidade precisa ser conhecida antes de interpretar a
> nuvem.

## Quando escolher outra forma

Se muitos pontos se sobrepõem, `view/bin2d` revela densidade em uma grade. Se o
eixo X representa tempo ou outra ordem explícita, `view/line` geralmente torna
a evolução mais legível.
