---
section: colecoes
title: Analisar resíduos
description: Compara valores previstos numéricos aos resíduos observados menos previstos.
collection: aprendizado
node: ml/residuals
related: ["ml/predict", "ml/evaluate"]
---

## O que o bloco faz

Compara valores previstos numéricos aos resíduos observados menos previstos.

## Quando usar

Use para procurar padrão, curvatura ou dispersão crescente em uma tarefa de regressão.

## Configuração

`resposta` nomeia a resposta numérica observada; `predito` nomeia previsão numérica (padrão `.pred`).

## Exemplo

```r
d <- data.frame(y = 1:4, .pred = c(1.1, 1.8, 3.2, 3.7))
trama.ml::tr_ml_residuals(d, "y")
```

## Como interpretar

Pontos distribuídos em torno de zero sem padrão visível são compatíveis com erros sem estrutura aparente; padrão indica aspectos a investigar, não uma prova isolada.
