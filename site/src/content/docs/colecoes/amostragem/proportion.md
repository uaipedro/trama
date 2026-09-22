---
title: Proporção
description: Estima a proporção da população em cada categoria de uma variável, com o erro do desenho.
section: colecoes
collection: amostragem
node: sampling/proportion
related: [sampling/size_proportion, sampling/plot_estimates, sampling/mean]
---

## O que o bloco faz

A proporção da população em cada categoria: a média ponderada do indicador
(1 se a unidade é da categoria, 0 se não). Com **Categoria** em branco, uma
linha por categoria; com uma categoria, só ela.

O card mostra em %; a tabela, em proporção (0 a 1). O intervalo é o de Wald com
t, que pode passar de 0 ou 1 em proporções extremas com amostra pequena — leia
junto do n.

## Quando usar

Use para estimar a fração populacional de uma categoria, uma ou todas as categorias de uma coluna.

## Configuração

- **Variável** — coluna categórica (texto, fator ou lógica).
- **Categoria** — o valor cuja proporção se quer; em branco, todas.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop") |>
  tr_add("proporcao", "sampling/proportion", variavel = "irrigada", nivel = "sim",
         por = "regiao", from = "amostra")
```

## Como interpretar

A tabela registra proporções entre 0 e 1, embora o card as apresente em porcentagem. Com categoria vazia, sai uma linha para cada categoria; intervalos de Wald podem ultrapassar 0 ou 1 em amostras pequenas. A saída é Estimativa `sampling/estimate` da proporção.

## Veja também

`sampling/size_proportion`, `sampling/plot_estimates`, `sampling/mean`.
