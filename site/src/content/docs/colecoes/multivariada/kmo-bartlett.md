---
title: KMO e Bartlett
description: "Resume a adequação da matriz de correlação à fatoração: KMO global, MSA por variável e teste de esfericidade."
section: colecoes
collection: multivariada
node: multi/kmo_bartlett
related: [multi/parallel, multi/factor_analysis]
---

## O que o bloco faz

O bloco `multi/kmo_bartlett` calcula KMO global, MSA por variável e esfericidade de Bartlett a partir das correlações entre variáveis. Devolve uma tabela. O bloco recebe `data/table`.

## Quando usar

Use **KMO e Bartlett** antes da fatoração para avaliar se há correlações comuns suficientes e identificar itens cuja inclusão enfraquece a matriz.

## Configuração

**Variáveis** — colunas numéricas, separadas por vírgula. Em branco, usa todas as colunas numéricas.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "questionario") |>
  tr_add("diag", "multi/kmo_bartlett", cols = "ans1, ans2, ans3, soc1, soc2", from = "dados")
```

A saída tabular contém os diagnósticos global e por variável para selecionar itens antes de fatorar.

## Como interpretar

KMO e MSA variam de 0 a 1; valores menores indicam menor adequação local/global. Bartlett significativo rejeita a matriz identidade, mas não mede a qualidade da solução fatorial.

## Veja também

- [`Análise paralela`](/trama/colecoes/multivariada/parallel/)
- [`Análise fatorial`](/trama/colecoes/multivariada/factor-analysis/)
