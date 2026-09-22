---
title: Comparar simulações
description: Desenha a distribuição das estimativas de vários desenhos, lado a lado, contra a verdade.
section: colecoes
collection: amostragem
node: sampling/plot_simulation
related: [sampling/simulate, sampling/plot_estimates]
---

## O que o bloco faz

Uma faixa por simulação ligada: o violino das estimativas, a caixa com a
mediana e os quartis, e a verdade como linha vertical. Ao lado de cada desenho,
o EP empírico (e o estimado), o viés e a cobertura.

A leitura é a da eficiência: com o mesmo n, a faixa mais ESTREITA é o desenho
mais preciso. Nas `fazendas` com n = 200, a estratificada por região fica mais
estreita que a AAS, e a de conglomerados por município, bem mais larga.

A porta aceita quantos cabos forem ligados.

## Quando usar

Use para comparar lado a lado as distribuições de estimativas produzidas por simulações de diferentes desenhos.

## Configuração

Só os de aparência.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("aas", "sampling/srs", n = 200L, from = "pop") |>
  tr_add("estratificada", "sampling/stratified", estrato = "regiao", n = 200L, from = "pop") |>
  tr_add("sim_aas", "sampling/simulate", variavel = "producao_t", repeticoes = 100L, from = "aas") |>
  tr_add("sim_estratificada", "sampling/simulate", variavel = "producao_t", repeticoes = 100L,
         from = "estratificada") |>
  tr_add("comparacao", "sampling/plot_simulation", from = "sim_aas") |>
  tr_link("sim_estratificada", "comparacao:simulacoes")
```

## Como interpretar

Cada distribuição representa um desenho; com n igual, faixas mais estreitas indicam menor variação das estimativas. O centro comparado à verdade mostra viés, e a cobertura informa se os intervalos atingem o nível nominal. A saída é Gráfico `view/plot` de simulações comparadas.

## Veja também

`sampling/simulate`.
