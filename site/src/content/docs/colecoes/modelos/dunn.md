---
title: Dunn
description: "Comparações de Dunn entre pares de grupos, o post hoc do Kruskal-Wallis, com p-valor ajustado."
section: colecoes
collection: modelos
node: models/dunn
category: testes
related: [models/kruskal, models/pairwise]
---

## O que o bloco faz

`models/dunn` compara todos os pares de grupos pelos postos médios (Dunn, 1964), usando os postos da amostra inteira e a correção de empates. A saída é um quadro de efeitos (`models/effects`), um par por linha, com a diferença dos postos médios, o `z` e o p-valor ajustado.

## Quando usar

Depois de um [Kruskal-Wallis](/trama/colecoes/modelos/kruskal/) que rejeita, para saber quais grupos diferem. Não refaz os postos a cada par, como faria o Wilcoxon repetido.

## Configuração

Resposta é a coluna numérica; Grupo, a dos grupos; Ajuste escolhe a correção dos p-valores: `holm` (padrão), `bonferroni`, `sidak` ou `nenhum`. A coluna `p_sem_ajuste` guarda o p de cada comparação isolada.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "InsectSprays") |>
  tr_add("kw", "models/kruskal", resposta = "count", grupo = "spray", from = "dados") |>
  tr_add("dunn", "models/dunn", resposta = "count", grupo = "spray", from = "dados")
```

No InsectSprays, os sprays A, B e F ficam juntos nos postos altos e C, D e E nos baixos; as comparações entre os dois blocos têm p ajustado pequeno.

## Como interpretar

Estimativa positiva quer dizer postos maiores no primeiro grupo do par. O p-valor que vale é o ajustado: com muitos grupos, o `p_sem_ajuste` subestima o risco de algum falso positivo na família de comparações.
