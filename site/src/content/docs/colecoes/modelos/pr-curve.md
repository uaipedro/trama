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

Com três ou mais classes e **Classe positiva** vazia, sai uma curva por classe contra as outras, com a AP e o acaso (a prevalência) de cada uma na legenda.

**Permitir avaliar o treino** — com `dados` vindos do [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/) (a tabela leva a marca de treino/teste), previsões das linhas de treino são recusadas (`tr_ml_error_train_eval`): a medida no treino é otimista. Ligado, avalia assim mesmo e acrescenta a nota de otimismo. Tabela sem a marca é avaliada como chega.

## Exemplo

```r
d <- data.frame(y = c("sim", "nao", "sim", "nao", "sim"), prob_sim = c(.9, .8, .7, .6, .2))
p <- trama.models::tr_models_pr_curve(dados = d, resposta = "y", probabilidade = "prob_sim")
p$data
```

## Como interpretar

No exemplo, os cortes dão (revocação, precisão) = (0,33; 1), (0,33; 0,5), (0,67; 0,67), (0,67; 0,5) e (1; 0,6). A AP soma ganho de revocação × precisão: (1 + 0,667 + 0,6)/3 = 0,756. A `area`, com a interpolação de Davis & Goadrich (2006), é 0,716 — menor porque entre dois cortes a precisão não varia em linha reta. Compare as duas com a prevalência, 0,6 aqui: um classificador ao acaso fica nessa linha.

### Previsões do treino

Se as linhas vierem do `treino` marcado pelo [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/), o bloco recusa por padrão (`tr_ml_error_train_eval`): a avaliação no treino é otimista e não mede generalização. Para medir o ajuste no treino de propósito (por exemplo, comparar com o teste e ver o sobreajuste), ligue `permitir_treino`; o resultado vem com um aviso e a nota de otimismo. O mesmo vale para uma tabela marcada como teste que traz linhas de fora dele (previsões do treino juntadas às do teste). Tabelas sem a marca do `ml/split` são avaliadas como chegam.
