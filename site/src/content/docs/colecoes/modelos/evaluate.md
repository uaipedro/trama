---
title: Avaliar previsões
description: "Erro de previsão (regressão) ou acerto, kappa e F1 (classificação), numa tabela de métricas."
section: colecoes
collection: modelos
node: models/evaluate
category: avaliar
related: [models/fit_stats, models/confusion, models/predict]
---

## O que o bloco faz

`models/evaluate` mede a previsão numa tabela `metrica`, `valor`, `n`. Regressão: `mae`, `rmse` e `r2` de previsão. Classificação: `accuracy`, `balanced_accuracy`, `macro_f1`, `kappa` e, com duas classes, `sensitivity` e `specificity` da classe positiva.

## Quando usar

Use para saber quanto o modelo erra num caso novo. `models/fit_stats` descreve o ajuste no treino; este bloco mede a previsão, por validação cruzada ou em dados que o modelo não viu.

## Configuração

Mesmas entradas opcionais da matriz de confusão. No modo tabela, resposta ou previsto categóricos fazem classificação; dois números, regressão. Linhas sem real ou previsto ficam fora, e `n` diz quantas contaram.

Precisão de classe nunca prevista é indefinida (0/0): sai NA e fica fora das médias macro e ponderada, como `zero_division = np.nan` do scikit-learn.

**Permitir avaliar o treino** — com `dados` vindos do [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/) (a tabela leva a marca de treino/teste), previsões das linhas de treino são recusadas (`tr_ml_error_train_eval`): a medida no treino é otimista. Ligado, avalia assim mesmo e acrescenta a nota de otimismo. Tabela sem a marca é avaliada como chega.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("reg", "models/lm", formula = "mpg ~ wt + hp", from = "dados") |>
  tr_add("cv", "models/evaluate", validacao = "cruzada", from = "reg")
```

## Como interpretar

`rmse` está na unidade da resposta. O `r2` de previsão pode ser negativo: o modelo prevê pior que a média. O `kappa` desconta o acerto que o acaso daria com as mesmas proporções.
