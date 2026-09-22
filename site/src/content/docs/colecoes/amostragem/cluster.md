---
title: Conglomerados
description: Sorteia conglomerados inteiros (municípios, escolas) e toma todas as suas unidades.
section: colecoes
collection: amostragem
node: sampling/cluster
related: [sampling/two_stage, sampling/size_cluster, sampling/simulate]
---

## O que o bloco faz

A amostra de conglomerados em UM estágio: sorteia conglomerados (municípios,
escolas, quarteirões) e entrevista todas as unidades de cada um. Não precisa de
cadastro das unidades — só da lista de conglomerados —, e junta as entrevistas
no espaço, que é onde está o custo de campo.

O preço: unidades do mesmo conglomerado se parecem, e o deff passa de 1 (veja
o card de `sampling/mean`). Nas `fazendas` por município, perto de 3,5.

- **iguais** — todo conglomerado com chance m/M; peso M/m.
- **proporcional ao tamanho** — chance proporcional ao número de unidades
  (PPS sistemática).

A variância é a do conglomerado último: a variação ENTRE os totais dos
conglomerados sorteados.

## Quando usar

Use quando o custo de campo favorece selecionar conglomerados inteiros e entrevistar todas as unidades deles.

## Configuração

- **Conglomerado** — coluna que identifica o conglomerado.
- **Conglomerados a sortear** — m (ou o **plano** de `sampling/size_cluster`).
- **Probabilidade** — `iguais` ou `proporcional ao tamanho`.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/cluster", conglomerado = "municipio", conglomerados = 12L,
         from = "pop")
```

## Como interpretar

Cada conglomerado sorteado contribui com todas as unidades. A variância é estimada entre os totais dos conglomerados; sem pelo menos dois conglomerados amostrados por estrato não há estimativa de variância. A saída é Amostra `sampling/sample` formada por conglomerados inteiros.

## Veja também

`sampling/two_stage`, `sampling/size_cluster`, `sampling/simulate`.
