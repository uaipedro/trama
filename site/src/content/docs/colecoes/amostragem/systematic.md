---
title: Sistemática
description: Sorteia um começo e toma uma unidade a cada N/n, ao longo do cadastro.
section: colecoes
collection: amostragem
node: sampling/systematic
related: [sampling/srs, sampling/stratified, sampling/simulate]
---

## O que o bloco faz

A amostra sistemática: com intervalo k = N/n, sorteia um começo entre 0 e k e
toma as unidades nas posições começo, começo + k, começo + 2k… É a amostra da
lista telefônica, da linha de produção, das árvores ao longo do talhão.

O intervalo é FRACIONÁRIO: com N/n não inteiro, sai exatamente n unidades.

**Ordenar por** uma coluna antes é estratificação implícita de graça: com as
fazendas em ordem de área, a amostra cobre pequenas, médias e grandes na
proporção certa, e a variância real cai. O estimador de variância, porém, é o
da AAS — a sistemática não tem estimador próprio sem suposição —, então o
erro padrão do card tende a ser CONSERVADOR quando a ordenação ajuda. Cuidado
com cadastro periódico (a mesma posição da semana a cada 7 linhas): aí a
sistemática engana, e a variância real SOBE.

## Quando usar

Use quando uma lista ordenada permite espalhar a amostra ao longo do cadastro, após conferir se não há periodicidade alinhada ao intervalo.

## Configuração

- **n**, **Fração** — como em `sampling/srs`.
- **Ordenar por** — coluna pela qual ordenar o cadastro antes (em branco: a
  ordem em que ele veio).

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/systematic", n = 200L, ordenar = "area_ha", from = "pop")
```

## Como interpretar

A saída contém exatamente n unidades escolhidas em intervalos N/n e seus pesos. A ordenação pode espalhar a amostra, mas periodicidade no cadastro alinhada ao intervalo pode alterar a representatividade. A saída é Amostra `sampling/sample` com unidades selecionadas sistematicamente.

## Veja também

`sampling/srs`, `sampling/stratified`, `sampling/simulate`.
