---
title: Matriz de correlação
description: Calcula correlações ou covariâncias entre variáveis, opcionalmente dentro de grupos.
section: colecoes
collection: multivariada
node: multi/correlation_matrix
related: [multi/plot_correlation, multi/kmo_bartlett]
---

## O que o bloco faz

O bloco `multi/correlation_matrix` calcula uma matriz de correlação ou covariância e a devolve em formato tabular, com nomes de variáveis nas linhas e colunas. O bloco recebe `data/table`.

## Quando usar

Use **Matriz de correlação** para obter valores exatos de associações ou comparar matrizes entre grupos antes de PCA, fatoração ou discriminante.

## Configuração

- **Variáveis** — colunas numéricas; em branco, todas menos Grupo.
- **Matriz** — `correlação` (padrão) ou `covariância`.
- **Método** — `pearson`, `spearman` ou `kendall`.
- **Grupo** — coluna opcional para calcular matrizes por nível; covariância requer Pearson.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "vinhos") |>
  tr_add("mat", "multi/correlation_matrix", cols = "alcool, flavonoides", grupo = "cultivar", from = "dados")
```

A tabela permite consultar os coeficientes exatos ou exportá-los para outra etapa.

## Como interpretar

Correlação varia entre −1 e 1, sem unidade; covariância mantém as unidades e sua diagonal contém variâncias. Com grupos, as matrizes aparecem empilhadas; Pearson inclui também a covariância/correlação combinada dentro dos grupos.

## Veja também

- [`Mapa de correlações`](/trama/colecoes/multivariada/plot-correlation/)
- [`KMO e Bartlett`](/trama/colecoes/multivariada/kmo-bartlett/)
