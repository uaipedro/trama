---
section: colecoes
title: SVM · vetores de suporte
description: Ajusta uma máquina de vetores de suporte para regressão ou classificação, com kernel configurável.
collection: aprendizado
node: ml/svm
related: ["ml/split", "models/predict", "models/evaluate", "models/roc"]
---

## O que o bloco faz

Ajusta uma máquina de vetores de suporte para regressão ou classificação, com kernel configurável.

## Quando usar

Use para ajustar uma fronteira linear ou não linear. A escala é estimada no treino e reaplicada nas previsões. Requer `e1071`.

## Configuração

`kernel` escolhe `linear`, `radial`, `polynomial` ou `sigmoid`; `cost` controla penalidade dos erros; `gamma` controla escala do kernel e não se aplica ao kernel linear. Recebe também `resposta`, `preditores`, `tarefa` e `seed`.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
m <- trama.ml::tr_ml_svm(d, resposta = "Species", preditores = "Petal.Length, Petal.Width", kernel = "radial")
trama.models::tr_models_predict(m, d[1:3, ])
```

## Como interpretar

`models/predict` acrescenta a classe `previsto` e probabilidades `prob_<classe>` quando disponíveis. Kernel não linear descreve fronteira menos diretamente que limiares de árvore.
