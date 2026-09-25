---
section: colecoes
title: Curva precisão-revocação
description: "Precisão × revocação em todos os cortes, com a precisão média (AP)."
collection: modelos
node: models/pr_curve
category: avaliar
related: [models/roc, models/predict, models/confusion]
---

## O que o bloco faz

Ordena as linhas pela probabilidade da classe positiva e mostra, em cada corte, a precisão (dos previstos positivos, quantos são) contra a revocação (dos positivos, quantos foram achados). Calcula a precisão média (AP) e desenha a prevalência como a linha do acaso.

## Quando usar

Quando a classe de interesse é rara. A ROC não muda com o desequilíbrio e pode parecer boa enquanto quase todos os alarmes são falsos; a curva precisão-revocação mostra isso diretamente (Saito & Rehmsmeier 2015).

## Configuração

Como a `models/roc`, tem três modos: ligue só um modelo (a curva sai da validação, `cruzada` ou `resubstituição`), um modelo e uma tabela de teste em `dados`, ou só uma tabela com a classe e a probabilidade. No modo tabela, `resposta` é a classe observada e `probabilidade` a coluna `prob_<classe>` (de `models/predict`). `positiva` escolhe a classe de interesse: vazia, no modo tabela é a do nome da coluna (com coluna de nome livre o bloco pede `positiva`); com modelo, o segundo nível. Com três ou mais classes é obrigatória, e a curva é ela contra as outras.

## Exemplo

```r
d <- data.frame(y = c("sim", "nao", "sim", "nao", "sim"), prob_sim = c(.9, .8, .7, .6, .2))
p <- trama.models::tr_models_pr_curve(dados = d, resposta = "y", probabilidade = "prob_sim")
p$data
```

## Como interpretar

No exemplo, os cortes dão (revocação, precisão) = (0,33; 1), (0,33; 0,5), (0,67; 0,67), (0,67; 0,5) e (1; 0,6). A AP soma ganho de revocação × precisão: (1 + 0,667 + 0,6)/3 = 0,756. A `area`, com a interpolação de Davis & Goadrich (2006), é 0,716 — menor porque entre dois cortes a precisão não varia em linha reta. Compare as duas com a prevalência, 0,6 aqui: um classificador ao acaso fica nessa linha.
