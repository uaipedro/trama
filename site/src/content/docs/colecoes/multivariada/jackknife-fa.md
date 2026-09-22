---
title: Jackknife da fatorial
description: Estima erro padrão e influência nas cargas rotacionadas ou comunalidades, reajustando sem cada observação.
section: colecoes
collection: multivariada
node: multi/jackknife_fa
related: [multi/factor_analysis, multi/fa_loadings, multi/parallel]
---

## O que o bloco faz

O bloco `multi/jackknife_fa` refaz a análise fatorial sem cada observação e estima erro padrão e influência de cargas rotacionadas ou comunalidades. O bloco recebe `multi/fa`.

## Quando usar

Use **Jackknife da fatorial** para localizar respondentes influentes e avaliar estabilidade dos itens que definem cada fator.

## Configuração

- **Estatística** — `cargas` (padrão) ou `comunalidades`.
- **Tabela** — `resumo` (padrão) ou `pseudovalores`.
- **Nível do intervalo** — 0,95 por padrão, entre 0,5 e 0,999.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "questionario") |>
  tr_add("amostra", "data/slice_head", n = 120L, from = "dados") |>
  tr_add("fa", "multi/factor_analysis", fatores = 3L, rotacao = "oblimin", from = "amostra") |>
  tr_add("jk", "multi/jackknife_fa", estatistica = "cargas", from = "fa")
```

O fluxo reproduz o recorte de 120 respondentes usado nos testes da coleção; o resumo quantifica a estabilidade das cargas entre as retiradas.

## Como interpretar

As réplicas alinham sinal e ordem dos fatores à solução original. Heywood ou falha de identificação numa réplica interrompe o cálculo e indica a linha. O limite é 5.000 linhas.

## Veja também

- [`Análise fatorial`](/trama/colecoes/multivariada/factor-analysis/)
- [`Cargas fatoriais`](/trama/colecoes/multivariada/fa-loadings/)
- [`Análise paralela`](/trama/colecoes/multivariada/parallel/)
