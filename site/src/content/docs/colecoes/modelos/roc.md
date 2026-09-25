---
title: Curva ROC
description: "Sensibilidade × especificidade em todos os cortes, com a AUC."
section: colecoes
collection: modelos
node: models/roc
category: avaliar
related: [models/confusion, models/evaluate]
---

## O que o bloco faz

`models/roc` desenha, para cada corte de probabilidade, a sensibilidade contra 1 − especificidade, com a área sob a curva (AUC) no subtítulo. A saída é `view/plot`.

## Quando usar

Use para comparar classificadores sem fixar um corte, sobretudo com classes desbalanceadas, em que a taxa de acerto engana.

## Configuração

Mesmas entradas opcionais da matriz de confusão (só modelo, modelo e dados, ou só dados). Classe positiva vazia é o segundo nível; com três ou mais classes sai uma curva por classe, contra as outras. No modo tabela, Probabilidade vazia lê `prob_<positiva>`, a coluna que `models/predict` escreve.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("logit", "models/glm", formula = "am ~ wt", familia = "binomial", from = "dados") |>
  tr_add("roc", "models/roc", validacao = "cruzada", from = "logit")
```

## Como interpretar

A diagonal tracejada é o acaso (AUC 0,5); quanto mais a curva sobe para o canto superior esquerdo, melhor. O ponto marca o corte do modelo (0,5, ou o `corte` da logística multivariada).
