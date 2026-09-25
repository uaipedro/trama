---
section: colecoes
title: Avaliar previsões
description: Calcula métricas comparando a resposta observada com a coluna prevista.
collection: aprendizado
node: ml/evaluate
related: ["ml/predict", "ml/confusion", "ml/split"]
---

## O que o bloco faz

Calcula métricas comparando a resposta observada com a coluna prevista.

## Quando usar

Avalie a saída de Prever conectada ao conjunto de teste, depois de definir o modelo.

## Configuração

`resposta` identifica a resposta observada; `predito` indica coluna prevista (padrão `.pred`); `tarefa` aceita `auto`, `regressao` ou `classificacao`.

## Exemplo

```r
d <- data.frame(y = c(1, 2, 3), .pred = c(1, 2, 4))
trama.ml::tr_ml_evaluate(d, resposta = "y")
```

## Como interpretar

Regressão retorna RMSE, MAE e R²; classificação retorna acurácia, acurácia balanceada e macro F1. R² fica indefinido para resposta constante.
