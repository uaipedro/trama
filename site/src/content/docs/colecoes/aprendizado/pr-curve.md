---
section: colecoes
title: Curva precisão-revocação
description: Traça precisão contra revocação para classificação binária e calcula a precisão média.
collection: aprendizado
node: ml/pr_curve
related: ["ml/roc", "ml/predict", "ml/confusion"]
---

## O que o bloco faz

Ordena as linhas pela probabilidade da classe positiva e mostra, em cada corte, a precisão (dos previstos positivos, quantos são) contra a revocação (dos positivos, quantos foram achados). Calcula a precisão média (AP) e desenha a prevalência como a linha do acaso.

## Quando usar

Quando a classe de interesse é rara. A ROC não muda com o desequilíbrio e pode parecer boa enquanto quase todos os alarmes são falsos; a curva precisão-revocação mostra isso diretamente (Saito & Rehmsmeier 2015).

## Configuração

`alvo` é a classe observada; `probabilidade` é a coluna `.prob_<classe>`; `positiva` escolhe a classe positiva — vazia usa a do nome da coluna e, com coluna de nome livre, o bloco pede `positiva`.

## Exemplo

```r
d <- data.frame(y = c("sim", "nao", "sim", "nao", "sim"), .prob_sim = c(.9, .8, .7, .6, .2))
p <- trama.ml::tr_ml_pr_curve(d, "y", ".prob_sim")
p$data
```

## Como interpretar

No exemplo, os cortes dão (revocação, precisão) = (0,33; 1), (0,33; 0,5), (0,67; 0,67), (0,67; 0,5) e (1; 0,6). A AP soma ganho de revocação × precisão: (1 + 0,667 + 0,6)/3 = 0,756. A `area`, com a interpolação de Davis & Goadrich (2006), é 0,716 — menor porque entre dois cortes a precisão não varia em linha reta. Compare as duas com a prevalência, 0,6 aqui: um classificador ao acaso fica nessa linha.
