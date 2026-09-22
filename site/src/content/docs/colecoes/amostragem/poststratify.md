---
title: Pós-estratificar
description: Ajusta os pesos para que cada grupo some o total conhecido da população.
section: colecoes
collection: amostragem
node: sampling/poststratify
related: [sampling/stratified, sampling/design, sampling/simulate]
---

## O que o bloco faz

A pós-estratificação: quando o total de cada grupo na população é conhecido (o
censo diz quantas fazendas há por região, quantas pessoas por sexo e idade),
os pesos da amostra são esticados ou encolhidos para que cada grupo some
exatamente o seu total:

    w*_i = w_i · N_g / N̂_g

Corrige o azar do sorteio (a AAS que veio com Sul demais) e parte da não
resposta (o grupo que respondeu menos pesa mais). A variância cai junto: o
erro padrão passa a usar os resíduos dentro dos pós-estratos, e a parte do erro
que era "quantos de cada grupo vieram" deixa de existir.

A tabela de **totais** tem uma linha por pós-estrato, com a coluna do
pós-estrato com o MESMO nome da coluna da amostra. Todo pós-estrato precisa de
total e de pelo menos uma unidade na amostra; grupo vazio se junta a um vizinho
antes, com um `data/mutate`.

Na simulação (`sampling/simulate`), a pós-estratificação é refeita em cada
amostra.

## Quando usar

Use quando o total populacional de cada grupo é conhecido e os grupos correspondem a uma variável de pós-estratificação.

## Configuração

- **Pós-estrato** — coluna do grupo, com o mesmo nome nas duas tabelas.
- **Coluna do total** — coluna da tabela de totais com o N do grupo.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("totais", "sampling/example", dataset = "estratos_fazendas") |>
  tr_add("aas", "sampling/srs", n = 200L, from = "pop") |>
  tr_add("ajustada", "sampling/poststratify", pos_estrato = "regiao", from = "aas") |>
  tr_link("totais", "ajustada:totais")
```

## Como interpretar

Os pesos ajustados somam o total populacional informado em cada pós-estrato. Todos os grupos precisam ter total conhecido e ao menos uma observação amostrada. A saída é Amostra `sampling/sample` com pesos pós-estratificados.

## Veja também

`sampling/stratified`, `sampling/design`, `sampling/simulate`.
