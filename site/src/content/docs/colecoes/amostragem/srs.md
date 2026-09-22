---
title: Aleatória simples
description: Sorteia n unidades do cadastro, todas com a mesma chance (AAS).
section: colecoes
collection: amostragem
node: sampling/srs
related: [sampling/systematic, sampling/stratified, sampling/size_mean, sampling/simulate]
---

## O que o bloco faz

A amostra aleatória simples (AAS): n unidades sorteadas do cadastro, todas com
a mesma probabilidade n/N. É a referência de todo o resto — o deff das outras
estimativas é medido contra ela.

O n vem de **n**, da **fração** (quando n é 0) ou do **plano** ligado na porta
`plano` (de `sampling/size_mean` ou `sampling/size_proportion`), que vence os
campos.

**Com reposição** a mesma unidade pode sair duas vezes, e a variância perde a
correção de população finita. Serve para ensinar e para bootstrap; em pesquisa
de verdade, sem reposição.

Todo peso é N/n.

## Quando usar

Use como método de referência quando todas as unidades do cadastro podem ter a mesma chance de seleção.

## Configuração

- **n** — quantas unidades sortear.
- **Fração** — a fração da população, usada quando n é 0.
- **Com reposição** — sortear com reposição.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/srs", n = 200L, from = "pop")
```

## Como interpretar

Cada linha selecionada recebe peso N/n. Com reposição, uma unidade pode aparecer mais de uma vez e a correção de população finita deixa de ser aplicada. A saída é Amostra `sampling/sample` com as linhas selecionadas e informações do desenho.

## Veja também

`sampling/systematic`, `sampling/stratified`, `sampling/size_mean`, `sampling/simulate`.
