---
title: Matriz de distância
description: Dissimilaridade entre as linhas da tabela — euclidiana, padronizada, D² de Mahalanobis ou Gower.
section: colecoes
collection: multivariada
node: multi/distance
related: [multi/cluster, multi/tocher, multi/correlation_matrix]
---

## O que o bloco faz

O bloco `multi/distance` calcula a distância entre cada par de linhas (genótipos, cultivares, acessos) a partir de uma `data/table`. A saída é do tipo `multi/dist`; o card é o mapa de calor da matriz, ordenado pelo UPGMA.

## Quando usar

É o primeiro passo de um estudo de diversidade genética: a mesma matriz alimenta o `multi/cluster` (UPGMA) e o `multi/tocher`.

## Configuração

- **Variáveis** — colunas numéricas; em branco, todas (na Gower, todas as colunas).
- **Distância** — `euclidiana padronizada` (padrão, cada variável com desvio 1), `euclidiana`, `mahalanobis` (sai como D², não a raiz; desconta a correlação entre caracteres) ou `gower` (caracteres mistos).
- **Rótulo das linhas** — coluna com o nome de cada linha, sem repetição.

## Exemplo

```r
tr_flow(reg) |>
  tr_add("usa", "multi/example", dataset = "USArrests") |>
  tr_add("d", "multi/distance", metodo = "mahalanobis", rotulo = "nome", from = "usa")
```

## Como interpretar

Blocos escuros na diagonal do mapa são grupos de indivíduos parecidos. Ligada numa entrada de tabela, a matriz vira a tabela larga (`rotulo` e uma coluna por indivíduo).
