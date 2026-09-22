---
title: Exemplo multivariado
description: Carrega dados reais ou simulados para PCA, fatoração e classificação.
section: colecoes
collection: multivariada
node: multi/example
related: [multi/pca, multi/factor_analysis]
---

## O que o bloco faz

O bloco `multi/example` carrega o conjunto selecionado e devolve sua tabela em `data/table`. Os dados incluem conjuntos reais do R e simulados com estrutura conhecida. O bloco não recebe entrada.

## Quando usar

Use **Exemplo multivariado** para iniciar um exemplo sem ler um arquivo externo. `USArrests` ilustra padronização; `questionario` tem três fatores simulados; `iris` e `pima` permitem classificação.

## Configuração

**Conjunto** — escolha `iris`, `USArrests`, `estados`, `caranguejos`, `vinhos`, `pima`, `questionario`, `harman_fisicas` ou `harman_24_testes`. Os dois conjuntos Harman são reconstruídos para reproduzir as matrizes publicadas.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "USArrests")
```

A saída é a tabela do conjunto escolhido, pronta para conectar ao bloco seguinte.

## Como interpretar

A tabela contém as colunas do conjunto escolhido. `USArrests` tem quatro medidas em escalas distintas; `questionario` tem os itens `ans*`, `soc*` e `org*`.

## Veja também

- [`Componentes principais`](/trama/colecoes/multivariada/pca/)
- [`Análise fatorial`](/trama/colecoes/multivariada/factor-analysis/)
