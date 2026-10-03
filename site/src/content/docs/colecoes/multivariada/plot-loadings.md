---
title: Mapa das cargas
description: Desenha mapa de calor das cargas fatoriais e oculta valores abaixo de um corte.
section: colecoes
collection: multivariada
node: multi/plot_loadings
related: [multi/fa_loadings, multi/factor_analysis, multi/parallel]
---

## O que o bloco faz

O bloco `multi/plot_loadings` apresenta as cargas fatoriais de padrão como mapa de calor; a saída é um gráfico. O bloco recebe `multi/fa`.

## Quando usar

Use **Mapa das cargas** para reconhecer agrupamentos de itens por fator e localizar cargas cruzadas na solução rotacionada.

## Configuração

- **Corte** — módulo mínimo da carga mostrada (0,3 por padrão).
- **Ordenar pelas cargas** — ligado por padrão. **Aspecto**, **Tema**, **Título**, **Rótulo X**, **Rótulo Y** e **Legenda** controlam o gráfico.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "questionario") |>
  tr_add("fa", "multi/factor_analysis", cols = "ans1, ans2, ans3, ans4, ans5, soc1, soc2, soc3, soc4, soc5, org1, org2, org3, org4, org5", fatores = 3L, from = "dados") |>
  tr_add("mapa", "multi/plot_loadings", corte = 0.3, from = "fa")
```

O mapa destaca as cargas de padrão com módulo igual ou superior ao corte.

## Como interpretar

Cor indica sinal e magnitude da carga. O Corte apenas oculta cargas pequenas na figura; não altera nem remove valores do modelo.

## Veja também

- [`Cargas fatoriais`](/trama/colecoes/multivariada/fa-loadings/)
- [`Análise fatorial`](/trama/colecoes/multivariada/factor-analysis/)
- [`Análise paralela`](/trama/colecoes/multivariada/parallel/)
