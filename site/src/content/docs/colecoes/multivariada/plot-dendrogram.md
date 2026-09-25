---
title: Dendrograma
description: Dendrograma do agrupamento com a linha de corte e os ramos coloridos por grupo.
section: colecoes
collection: multivariada
node: multi/plot_dendrogram
related: [multi/cluster, multi/tocher]
---

## O que o bloco faz

O bloco `multi/plot_dendrogram` desenha a árvore de um `multi/cluster`: altura de junção no eixo, ramos abaixo do corte na cor do grupo, corte tracejado e a correlação cofenética no subtítulo. Num k-means, mostra os grupos no plano das duas primeiras componentes.

## Configuração

- **Grupos** — quantos grupos colorir; 0 usa os do agrupamento.
- **Horizontal** — deita a árvore, melhor com muitos nomes.

## Exemplo

```r
tr_flow(reg) |>
  tr_add("usa", "multi/example", dataset = "USArrests") |>
  tr_add("ag", "multi/cluster", rotulo = "nome", metodo = "Ward.D2", from = "usa") |>
  tr_add("dend", "multi/plot_dendrogram", grupos = 4L, horizontal = TRUE, from = "ag")
```

## Como interpretar

Ramos longos logo abaixo do corte indicam grupos nítidos; folhas que se juntam muito alto são indivíduos divergentes.
