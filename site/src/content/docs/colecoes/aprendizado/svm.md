---
section: colecoes
title: SVM · vetores de suporte
description: Ajusta uma máquina de vetores de suporte para regressão ou classificação, com kernel configurável.
collection: aprendizado
node: ml/svm
related: ["ml/split", "ml/predict", "ml/evaluate", "ml/roc"]
---

## O que o bloco faz

Ajusta uma máquina de vetores de suporte para regressão ou classificação, com kernel configurável.

## Quando usar

Use para ajustar uma fronteira linear ou não linear. A escala é estimada no treino e reaplicada nas previsões. Requer `e1071`.

## Configuração

`kernel` escolhe `linear`, `radial`, `polynomial` ou `sigmoid`; `cost` controla penalidade dos erros; `gamma` controla escala do kernel e não se aplica ao kernel linear. Recebe também `alvo`, `cols`, `tarefa` e `seed`.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
m <- trama.ml::tr_ml_svm(d, alvo = "Species", cols = "Petal.Length, Petal.Width", kernel = "radial")
trama.ml::tr_ml_predict(m, d[1:3, ])
```

## Como interpretar

Prever acrescenta a classe `.pred` e probabilidades `.prob_<classe>` quando disponíveis. Kernel não linear descreve fronteira menos diretamente que limiares de árvore.

### Teste recusado

A saída `teste` do [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/) é recusada aqui (`tr_ml_error_test_leak`): ajustar nela treinaria no teste. Ligue a saída `treino`; o teste vai só ao `ml/predict`.
