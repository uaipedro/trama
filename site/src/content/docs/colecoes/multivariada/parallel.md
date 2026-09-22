---
title: Análise paralela
description: Compara autovalores observados com quantis de autovalores de dados aleatórios para orientar a retenção de fatores.
section: colecoes
collection: multivariada
node: multi/parallel
related: [multi/kmo_bartlett, multi/factor_analysis]
---

## O que o bloco faz

O bloco `multi/parallel` compara os autovalores observados aos quantis de autovalores de matrizes aleatórias com o mesmo tamanho. Devolve uma tabela com autovalores observados, referências simuladas e a indicação `reter`. O bloco recebe `data/table`.

## Quando usar

Use **Análise paralela** para estimar quantos componentes ou fatores explicam mais variância que dados sem estrutura correlacional.

## Configuração

- **Variáveis** — colunas numéricas; em branco, todas.
- **Repetições** — número de matrizes aleatórias (100 por padrão, de 10 a 10.000).
- **Percentil** — quantil de referência (95 por padrão, de 50 a 99).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "questionario") |>
  tr_add("pa", "multi/parallel", cols = "ans1, ans2, ans3, soc1, soc2, soc3", from = "dados")
```

A tabela alinha autovalores observados e aleatórios para comparar critérios de retenção.

## Como interpretar

Considere os autovalores observados acima da referência aleatória como candidatos a retenção; confira também interpretabilidade. `questionario` tem três fatores plantados.

## Veja também

- [`KMO e Bartlett`](/trama/colecoes/multivariada/kmo-bartlett/)
- [`Análise fatorial`](/trama/colecoes/multivariada/factor-analysis/)
