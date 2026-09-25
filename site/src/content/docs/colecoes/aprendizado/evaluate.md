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

`alvo` identifica resposta; `predito` indica coluna prevista (padrão `.pred`); `tarefa` aceita `auto`, `regressao` ou `classificacao`.

## Exemplo

```r
d <- data.frame(y = c(1, 2, 3), .pred = c(1, 2, 4))
trama.ml::tr_ml_evaluate(d, alvo = "y")

c3 <- data.frame(y = c("a", "a", "a", "b", "b", "c"), .pred = c("a", "a", "b", "b", "c", "c"))
trama.ml::tr_ml_evaluate(c3, alvo = "y")
```

## Como interpretar

Regressão retorna RMSE, MAE e R²; R² fica indefinido para resposta constante. Classificação retorna acurácia, acurácia balanceada (média das revocações), macro F1, kappa de Cohen, precisão e revocação macro, as versões ponderadas pelo suporte e, por classe, precisão, revocação e F1, com o suporte em `n`. No segundo exemplo, a classe `a` tem precisão 1, revocação 0,667 e F1 0,8; a acurácia é 0,667, a balanceada 0,722, o macro F1 0,656 e o kappa 0,5 — a concordância esperada pelas marginais é 1/3. Uma classe nunca prevista tem precisão 0 por convenção.
