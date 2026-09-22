---
title: Resumo
description: Inspecione tipos, faltantes, valores distintos e exemplos de cada coluna.
section: colecoes
collection: dados
node: data/summary
category: conhecer
order: 3
related: [data/read_csv, data/filter, data/group_summarise]
---

## Finalidade

`data/summary` descreve cada coluna da tabela. O resultado informa tipo,
quantidade de faltantes, valores distintos, extremos e um exemplo.

## Quando usar

Use o resumo logo após uma fonte e antes de operações que descartam ou
convertem dados. Ele permite conferir se a leitura interpretou as colunas como
esperado.

> **Antes de continuar**
>
> Inspecionar não altera a tabela original. O bloco produz outra tabela, de
> diagnóstico, enquanto a tabela lida continua disponível para outras conexões
> no fluxo.

## Próximo passo

Ordene o resumo por faltantes para localizar colunas que exigem decisão. Use
`data/convert` quando o tipo não corresponde ao significado da coluna e
`data/drop_na` ou `data/replace_na` quando a ausência exige tratamento.
