---
title: Gráfico das margens
description: Margem de erro por nível e cenário, com a linha da meta.
section: colecoes
collection: amostragem
node: sampling/plot_margins
related: [sampling/margin_levels, sampling/referral]
---

## O que o bloco faz

Desenha a tabela de `sampling/margin_levels` ou de `sampling/referral`: uma
linha de painéis por tipo de nível (total, cada nível intermediário, unidades),
a margem de cada nível em pontos percentuais no eixo horizontal, e a meta como
linha tracejada. Os níveis vêm ordenados da menor margem (em cima) para a maior.

Com a tabela de `sampling/referral`, a cor é o número de convidados e há uma
coluna de painéis por ICC: o que está à esquerda da linha tracejada atinge a
meta. Para muitas unidades, escolha a proporção `3:4` ou `1:1`.

## Quando usar

Use para comparar visualmente as margens calculadas por `sampling/margin_levels` ou `sampling/referral` com uma meta.

## Configuração

- **Meta** — a margem máxima aceitável, em pontos percentuais.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("unidades", "sampling/example", dataset = "escolas_resumo") |>
  tr_add("com_n", "data/mutate", name = "n", expr = "10", from = "unidades") |>
  tr_add("margens", "sampling/referral", unidade = "escola", tamanho = "alunos", n = "n",
         grupos = "rede", convidados = "0, 2, 4", icc = "0.1", from = "com_n") |>
  tr_add("grafico", "sampling/plot_margins", meta = 5, from = "margens")
```

## Como interpretar

O eixo apresenta margens em pontos percentuais e a linha tracejada marca a meta. Pontos à esquerda da meta atendem à precisão definida; compare painéis e cenários correspondentes. A saída é Gráfico `view/plot` das margens e da meta.

## Veja também

`sampling/margin_levels`, `sampling/referral`.
