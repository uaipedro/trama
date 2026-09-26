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

**Confiança do IC da AUC** — cada AUC sai com o intervalo de DeLong, DeLong & Clarke-Pearson (1988), no nível pedido (padrão 0,95): assintótico, cortado em [0, 1]. Com AUC 0 ou 1 (variância zero) ou menos de duas linhas numa classe, o intervalo sai indisponível e a legenda explica, sem recusar a curva. Na curva de uma classe, o círculo vazio marca o corte de Youden (1950), que maximiza sensibilidade + especificidade − 1. Com três ou mais classes e positiva vazia, o subtítulo traz a AUC multiclasse M de Hand & Till (2001), a média das AUCs dos pares de classes, que não depende das proporções.

**Permitir avaliar o treino** — com `dados` vindos do [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/) (a tabela leva a marca de treino/teste), previsões das linhas de treino são recusadas (`tr_ml_error_train_eval`): a medida no treino é otimista. Ligado, avalia assim mesmo e acrescenta a nota de otimismo. Tabela sem a marca é avaliada como chega.

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
