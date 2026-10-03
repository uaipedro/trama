---
title: Renomear
description: Troque nomes de colunas sem alterar seus valores.
section: colecoes
collection: dados
node: data/rename
category: limpar
order: 14
related: [data/clean_names, data/select]
---

## O que o bloco faz

Substitui nomes de colunas segundo duas listas posicionais; os valores e a ordem das colunas permanecem iguais.

## Quando usar

Quando poucas colunas precisam receber nomes escolhidos para as etapas seguintes.

## Configuração

**De** (`from`) lista nomes atuais e **Para** (`to`) lista os novos nomes na mesma ordem. As listas devem ter o mesmo número de itens.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("ar", "data/example", dataset = "airquality") |>
  tr_add("nomes", "data/rename", to = "ozonio, radiacao", from = "ar") |>
  tr_set("nomes", from = "Ozone, Solar.R")
```

## Como interpretar

A saída mantém as observações e renomeia `Ozone` para `ozonio` e `Solar.R` para `radiacao`; as outras colunas conservam seus nomes.
