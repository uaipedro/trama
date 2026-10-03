---
title: Converter tipo
description: Converta colunas para o tipo adequado à operação seguinte.
section: colecoes
collection: dados
node: data/convert
category: limpar
order: 17
related: [data/summary, data/mutate]
---

## O que o bloco faz

Converte as colunas selecionadas para o tipo indicado, interpretando números e datas segundo os parâmetros de formato.

## Quando usar

Depois da leitura, quando uma coluna numérica ou temporal foi reconhecida como texto.

## Configuração

**Colunas** (`cols`) seleciona as variáveis; **Tipo** (`type`) escolhe número, inteiro, texto, data, hora, lógico ou fator. **Decimal** (`decimal`) define `.` ou `,`; **Formato da data** (`format`) informa a máscara, como `%d/%m/%Y`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("medidas", "data/generate",
         expr = "tibble::tibble(valor = c('1.234,50', '980,25'))") |>
  tr_add("numeros", "data/convert", cols = "valor", decimal = ",",
         from = "medidas") |>
  tr_set("numeros", type = "numero")
```

## Como interpretar

Os textos `1.234,50` e `980,25` passam a números `1234.5` e `980.25`, respeitando a vírgula decimal e o ponto de milhar.
