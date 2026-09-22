---
title: Gerar dados
description: Monte uma tabela avaliando expressões R com tamanho e semente definidos.
section: colecoes
collection: dados
node: data/generate
category: fonte
order: 9
related: [data/example, data/summary]
---

## O que o bloco faz

Avalia expressões R para formar colunas de uma tabela simulada. Colunas nomeadas podem usar os valores criados antes delas.

## Quando usar

Quando precisa simular dados para testar um fluxo ou construir um exemplo controlado.

## Configuração

**Tamanho (n)** (`n`) fica disponível como variável nas expressões. **Colunas** (`expr`) recebe expressões separadas por vírgula; atribua nomes para criar várias colunas. A semente é definida na instância do nó.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)

tr_flow(reg) |>
  tr_add("sim", "data/generate", n = 6L,
         expr = "dose = rep(c(0, 5), each = n / 2), resposta = 12 + 2 * dose + rnorm(n)") |>
  tr_add("perfil", "data/summary", from = "sim")
```

## Como interpretar

Expressões nomeadas criam colunas na ordem escrita e as seguintes podem usar as anteriores. A expressão simples sem nome cria a coluna `valor`; `n` é variável disponível, não garantia do número de linhas.
