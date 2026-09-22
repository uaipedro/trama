---
title: Cargas fatoriais
description: Produz cargas de padrão ou estrutura, ou correlações entre fatores.
section: colecoes
collection: multivariada
node: multi/fa_loadings
related: [multi/factor_analysis, multi/plot_loadings, multi/parallel]
---

## O que o bloco faz

O bloco `multi/fa_loadings` extrai uma tabela de cargas de padrão, de estrutura ou de correlações entre fatores, além dos diagnósticos por variável. O bloco recebe `multi/fa`.

## Quando usar

Use **Cargas fatoriais** para interpretar a relação item-fator, avaliar comunalidades ou verificar se os fatores se correlacionam.

## Configuração

- **Matriz** — `padrão` (padrão), `estrutura` ou `correlação entre fatores`.
- **Ordenar pelas cargas** — ligado por padrão; agrupa variáveis por fator dominante.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "questionario") |>
  tr_add("fa", "multi/factor_analysis", cols = "ans1, ans2, ans3, ans4, ans5, soc1, soc2, soc3, soc4, soc5, org1, org2, org3, org4, org5", fatores = 3L, from = "dados") |>
  tr_add("cargas", "multi/fa_loadings", matriz = "padrão", from = "fa")
```

A tabela mostra cargas, comunalidades e unicidades dos itens na rotação escolhida.

## Como interpretar

A matriz padrão expressa efeitos únicos dos fatores; a estrutura mostra correlações item-fator e pode diferir na rotação oblíqua. Comunalidade é a fração explicada; unicidade é o restante; complexidade maior indica carga distribuída entre fatores.

## Veja também

- [`Análise fatorial`](/trama/colecoes/multivariada/factor-analysis/)
- [`Mapa das cargas`](/trama/colecoes/multivariada/plot-loadings/)
- [`Análise paralela`](/trama/colecoes/multivariada/parallel/)
