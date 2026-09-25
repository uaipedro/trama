---
title: Agrupamento
description: Agrupamento hierárquico (UPGMA, Ward, completo, simples) ou k-means, com corte em k grupos e correlação cofenética.
section: colecoes
collection: multivariada
node: multi/cluster
related: [multi/distance, multi/plot_dendrogram, multi/tocher]
---

## O que o bloco faz

O bloco `multi/cluster` junta os indivíduos em grupos. Recebe a tabela (entrada `dados`, distância euclidiana nas variáveis) **ou** uma matriz do `multi/distance` (entrada `distancia`). O card é o dendrograma com o corte.

## Quando usar

Diversidade genética entre genótipos, tipologia de propriedades, qualquer pergunta de "quem se parece com quem".

## Configuração

- **Variáveis**, **Padronizar**, **Rótulo das linhas** — só quando parte da tabela.
- **Método** — `UPGMA` (ligação média, o mais usado em melhoramento), `Ward.D2`, `completo`, `simples` ou `k-means` (só a partir da tabela, com a semente do nó).
- **Grupos** — o k do corte.

## Exemplo

```r
tr_flow(reg) |>
  tr_add("usa", "multi/example", dataset = "USArrests") |>
  tr_add("d", "multi/distance", rotulo = "nome", from = "usa") |>
  tr_add("ag", "multi/cluster", metodo = "UPGMA", grupos = 4L, from = "d")
```

## Como interpretar

A correlação cofenética (no subtítulo e no resumo) mede quanto a árvore preserva as distâncias; acima de 0,7 é boa representação. Ligado numa tabela, o agrupamento vira os dados com a coluna `grupo` (G1, G2...).
