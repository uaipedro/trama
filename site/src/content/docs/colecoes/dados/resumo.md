---
title: Resumo
description: Leia um perfil de cada coluna para conhecer tipos, faltantes, diversidade e valores observados.
section: colecoes
collection: dados
node: data/summary
category: conhecer
order: 3
related: [data/read_csv, data/group_summarise]
---

## O que o bloco faz

`data/summary` organiza um perfil da tabela, com uma linha para cada coluna.
Ele apresenta tipo, quantidade de valores faltantes, quantidade de valores
distintos, mínimo, máximo e um exemplo observado.

O perfil sai como tabela com os campos `coluna`, `tipo`, `faltantes`,
`distintos`, `minimo`, `maximo` e `exemplo`; outros nós podem ordenar essa
tabela para priorizar colunas com ausências.

## Quando usar

Use **Resumo** depois de ler ou gerar dados e antes de definir conversões ou
tratamentos de ausências. Em `airquality`, por exemplo, o perfil mostra quantos
dias não têm medida de ozônio e permite comparar faltantes entre colunas.

Se `Solar.R` e `Ozone` tiverem faltantes, ordene o perfil por `faltantes` para
priorizar as colunas que exigem decisão antes do cálculo.

> **Antes de continuar**
>
> Cada linha do perfil representa uma coluna dos dados. Para comparar medidas
> entre grupos — como média, mediana ou total por região — **Agrupar e resumir**
> produz uma linha por grupo. As duas tabelas respondem perguntas diferentes:
> uma descreve a estrutura das colunas; a outra reúne medidas calculadas.

## Configuração

O bloco recebe a tabela pela entrada e não tem parâmetros. Conecte-o depois de uma fonte ou transformação que produza `data/table`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("perfil", "data/summary", from = "ar") |>
  tr_add("mais_faltantes", "data/arrange", cols = "faltantes", desc = TRUE,
         from = "perfil")
```

A tabela `perfil` tem uma linha por coluna de `airquality`; `mais_faltantes`
ordena essas descrições da maior para a menor quantidade de valores ausentes.
O nó `ar` continua disponível para outras ramificações do fluxo.

## Como interpretar

`faltantes` conta `NA`; `distintos` conta valores observados diferentes. Em
`airquality`, os extremos de `Ozone` descrevem apenas dias medidos, enquanto
`exemplo` mostra um valor observado. `Month` e `Day` identificam o calendário
das observações, não uma medida contínua.
