---
title: Margem por nível
description: Com n entrevistas em cada unidade, qual a margem no total, em cada nível intermediário e em cada unidade?
section: colecoes
collection: amostragem
node: sampling/margin_levels
related: [sampling/referral, sampling/plot_margins, sampling/question_margins]
---

## O que o bloco faz

A margem de erro de uma proporção em todos os níveis de agregação de uma vez: o
total (o país), cada nível intermediário (as macrorregiões) e cada unidade (as
capitais), a partir de uma tabela com a população e o n previsto de cada
unidade.

Numa unidade é a margem de uma AAS de n. Num agregado, as unidades são estratos:
a estimativa pondera cada uma pela sua população, e a margem sai da variância
estratificada, Σ W_h²(1 − f_h)·p(1 − p)/n_h.

**O preço de n igual em unidades desiguais.** Com 25 entrevistas em São Paulo e
25 em Palmas, a média nacional precisa dar a São Paulo o peso da sua população,
e a amostra fica desbalanceada em relação a esses pesos. O custo é o deff de
Kish, n · Σ W_h²/n_h, na coluna `deff_ponderacao`: 1 com alocação proporcional
à população, e tanto maior quanto mais desigual. É por isso que 675
entrevistas em 27 capitais valem, no total, bem menos que 675 aleatórias.

**Deff de agrupamento** multiplica tudo, para quando as entrevistas vêm em
grupos parecidos entre si (escolas, redes de indicação).

## Quando usar

Use para prever a margem no total, nos grupos intermediários e em cada unidade antes de campo.

## Configuração

- **Deff de agrupamento** — efeito de conglomerado além da ponderação (1:
  nenhum).

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("unidades", "sampling/example", dataset = "escolas_resumo") |>
  tr_add("com_n", "data/mutate", name = "n", expr = "10", from = "unidades") |>
  tr_add("margens", "sampling/margin_levels", unidade = "escola", tamanho = "alunos",
         n = "n", grupos = "rede", from = "com_n")
```

## Como interpretar

A tabela contém linhas para o total, níveis intermediários e unidades, com n, efeito de ponderação, efeito total, n efetivo e margem. A margem de cada unidade considera sua população e número de entrevistas previstos. A saída é Tabela `data/table` com margens por total, grupo e unidade.

## Veja também

`sampling/referral`, `sampling/plot_margins`, `sampling/question_margins`.
