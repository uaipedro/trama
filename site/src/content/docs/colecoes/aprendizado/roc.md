---
section: colecoes
title: Curva ROC
description: Traça sensibilidade contra taxa de falsos positivos para classificação binária e calcula AUC.
collection: aprendizado
node: ml/roc
related: ["ml/predict", "ml/confusion", "ml/evaluate"]
---

## O que o bloco faz

Traça sensibilidade contra taxa de falsos positivos para classificação binária e calcula AUC.

## Quando usar

Use as probabilidades de uma classe para comparar a discriminação ao longo de limiares possíveis.

## Configuração

`alvo` é a classe observada; `probabilidade` é coluna `.prob_<classe>`; `positiva` escolhe a classe positiva; vazia usa a classe do nome da coluna `.prob_<classe>` e, com coluna de nome livre, o bloco pede `positiva` em vez de adivinhar; `confianca` é o nível do intervalo da AUC (padrão 0,95).

## Exemplo

```r
d <- data.frame(y = factor(c("nao", "sim", "nao", "sim")), .prob_sim = c(.1, .8, .4, .7))
trama.ml::tr_ml_roc(d, "y", ".prob_sim", "sim")
```

## Como interpretar

AUC é a área sob a curva, incluída no gráfico. Ela resume ordenação das classes e deve ser lida junto à distribuição de classes e ao custo das decisões.

### Intervalo da AUC e corte de Youden

O gráfico traz o intervalo de confiança da AUC pelo método de DeLong, DeLong & Clarke-Pearson (1988) e marca em vermelho o corte de Youden (1950), o que maximiza J = sensibilidade + especificidade − 1. Com `y = nao, sim, nao, sim, nao, sim` e `.prob_sim = .1, .8, .4, .7, .6, .3`, a AUC é 0,778 com IC 95% de 0,291 a 1 (erro-padrão 0,248; truncado em 1) e o corte de Youden é P ≥ 0,7, com J = 0,667. Com tão poucas linhas o intervalo é largo e só aproximado; o corte escolhido nas mesmas linhas em que é lido sai otimista.

### Previsões do treino

Se as linhas vierem do `treino` marcado pelo [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/), o bloco recusa por padrão (`tr_ml_error_train_eval`): a avaliação no treino é otimista e não mede generalização. Para medir o ajuste no treino de propósito (por exemplo, comparar com o teste e ver o sobreajuste), ligue `permitir_treino`; o resultado vem com um aviso e a nota de otimismo. O mesmo vale para uma tabela marcada como teste que traz linhas de fora dele (previsões do treino juntadas às do teste). Tabelas sem a marca do `ml/split` são avaliadas como chegam.
