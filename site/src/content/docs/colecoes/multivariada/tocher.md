---
title: Tocher
description: Método de otimização de Tocher sobre uma matriz de distância.
section: colecoes
collection: multivariada
node: multi/tocher
related: [multi/distance, multi/cluster]
---

## O que o bloco faz

O bloco `multi/tocher` aplica o método de otimização de Tocher (Rao, 1952; como em Cruz, Regazzi e Carneiro) a uma matriz do `multi/distance`. O limite θ é a maior das menores distâncias; cada grupo começa pelo par mais próximo que sobra e recebe indivíduos enquanto a distância média do candidato ao grupo não passar de θ. Se o par de abertura já está acima de θ, cada indivíduo restante vira um grupo de um e o método termina (como o `tocher()` original do biotools).

## Exemplo

```r
tr_flow(reg) |>
  tr_add("usa", "multi/example", dataset = "USArrests") |>
  tr_add("d", "multi/distance", metodo = "mahalanobis", rotulo = "nome", from = "usa") |>
  tr_add("toc", "multi/tocher", from = "d")
```

## Como interpretar

A saída é uma tabela com uma linha por grupo: `grupo`, `n`, `membros`, `distancia_media` e `theta`. Cruzamentos entre genótipos de grupos diferentes prometem mais variabilidade.
