---
title: Preencher faltantes
description: Substitua faltantes nas colunas indicadas por um valor definido.
section: colecoes
collection: dados
node: data/replace_na
category: limpar
order: 16
related: [data/drop_na, data/convert]
---

## O que o bloco faz

Substitui valores `NA` nas colunas escolhidas pelo valor informado, mantendo as linhas da tabela.

## Quando usar

Quando o domínio atribui significado explícito ao valor de reposição, como zero para quantidade não registrada que representa ausência de ocorrências.

## Configuração

**Colunas** (`cols`) escolhe onde preencher; **Substituir por** (`value`) informa o valor, convertido conforme o tipo da coluna.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("pedidos", "data/generate", n = 3L,
         expr = "pedido = 1:3, desconto = c(0, NA, 15)") |>
  tr_add("sem_cupom_zero", "data/replace_na", cols = "desconto", value = "0", from = "pedidos")
```

## Como interpretar

Neste exemplo, `NA` em `desconto` significa que o pedido não usou cupom; depois do preenchimento, essa coluna contém `0` nesses pedidos. A regra só é válida porque o exemplo define explicitamente esse significado.
