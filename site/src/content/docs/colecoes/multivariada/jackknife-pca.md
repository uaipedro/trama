---
title: Jackknife da PCA
description: Estima viés, erro padrão e influência de observações em autovalores, proporções ou cargas da PCA.
section: colecoes
collection: multivariada
node: multi/jackknife_pca
related: [multi/pca, multi/pca_loadings, multi/biplot]
---

## O que o bloco faz

O bloco `multi/jackknife_pca` remove cada observação sucessivamente, refaz a PCA e estima viés, erro padrão, intervalo e influência em autovalores, proporções ou cargas. O bloco recebe `multi/pca`.

## Quando usar

Use **Jackknife da PCA** para avaliar se casos individuais alteram as conclusões sobre variância explicada ou associação variável-componente.

## Configuração

- **Estatística** — `autovalores` (padrão), `proporção` ou `cargas`.
- **Tabela** — `resumo` (padrão) ou `pseudovalores`.
- **Confiança do intervalo** (`confianca`) — 0,95 por padrão, entre 0,5 e 0,999. Até a versão 1 do bloco o param se chamava `nivel`; fluxo salvo com `nivel` abre migrado.
- **Grupo (apagar-um-grupo)** — em branco (padrão), tira uma linha por vez. Com dados em conglomerados (várias linhas do mesmo talhão, animal ou lote), informe a coluna do conglomerado: cada réplica tira o grupo inteiro e o erro padrão usa o número de grupos G no lugar de n, com intervalo t(G − 1) — a variância JK1 de amostragem (Shao & Tu, 1995; Kott, 2001). Viés, corrigida e pseudovalores com G só valem com grupos do mesmo tamanho: com tamanhos diferentes saem NA, o intervalo centra na estimativa da amostra toda e a coluna `nota` diz por quê. Com menos de 5 grupos o bloco avisa (o EP tem poucos graus de liberdade). Os pseudovalores saem um por grupo.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", cols = "Murder, Assault, UrbanPop, Rape", from = "dados") |>
  tr_add("jk", "multi/jackknife_pca", estatistica = "cargas", from = "pca")
```

O resumo quantifica a estabilidade das cargas entre as 50 réplicas de retirada.

## Como interpretar

Resumo agrega estimativas e réplicas; pseudovalores expõem o efeito de cada retirada. Autovalores próximos permitem troca na ordem dos componentes e podem inflar incerteza das cargas. O limite é 5.000 linhas.

## Veja também

- [`Componentes principais`](/trama/colecoes/multivariada/pca/)
- [`Cargas da PCA`](/trama/colecoes/multivariada/pca-loadings/)
- [`Biplot`](/trama/colecoes/multivariada/biplot/)
