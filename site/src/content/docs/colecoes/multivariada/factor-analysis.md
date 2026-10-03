---
title: Análise fatorial
description: Estima fatores latentes comuns, aplica rotação e pode calcular escores fatoriais.
section: colecoes
collection: multivariada
node: multi/factor_analysis
related: [multi/parallel, multi/fa_loadings, multi/plot_loadings]
---

## O que o bloco faz

O bloco `multi/factor_analysis` estima fatores comuns às variáveis, aplica rotação e pode calcular escores; o resultado é um modelo fatorial. O bloco recebe `data/table`.

## Quando usar

Use **Análise fatorial** para representar construtos latentes que explicam a covariação entre itens, como ansiedade e sociabilidade em questionários.

## Configuração

- **Variáveis** — colunas numéricas; em branco, todas.
- **Fatores** — 2 por padrão.
- **Método** — `ml` (máxima verossimilhança, padrão) ou `paf` (eixo principal).
- **Rotação** — `varimax` por padrão; opções incluem `nenhuma`, `quartimax`, `equamax`, `promax` e `oblimin`.
- **Normalização de Kaiser** — ligada por padrão.
- **Escores** — `regressão` por padrão, `bartlett` ou `nenhum`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "questionario") |>
  tr_add("fa", "multi/factor_analysis", cols = "ans1, ans2, ans3, ans4, ans5, soc1, soc2, soc3, soc4, soc5, org1, org2, org3, org4, org5", fatores = 3L, from = "dados")
```

O ajuste mantém três fatores e pode ser conectado às tabelas e mapas de cargas.

## Como interpretar

Cargas associam itens a fatores. Na rotação oblíqua os fatores podem se correlacionar; matriz padrão e matriz estrutura respondem a perguntas diferentes. Solução Heywood indica variância única ou comunalidade problemática.

## Veja também

- [`Análise paralela`](/trama/colecoes/multivariada/parallel/)
- [`Cargas fatoriais`](/trama/colecoes/multivariada/fa-loadings/)
- [`Mapa das cargas`](/trama/colecoes/multivariada/plot-loadings/)
