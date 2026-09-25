---
section: colecoes
title: Ajustar hiperparâmetros
description: Avalia configurações por validação cruzada nos dados recebidos e reajusta a melhor configuração em todas as linhas.
collection: aprendizado
node: ml/tune
related: ["ml/split", "ml/predict", "ml/evaluate", "ml/tuning_plot"]
---

## O que o bloco faz

Avalia configurações por validação cruzada nos dados recebidos e reajusta a melhor configuração em todas as linhas.

## Quando usar

Conecte apenas treino. Mantenha o teste separado até a avaliação final.

## Configuração

`modelo` aceita CART, FIGS, forest, SVM ou XGBoost; `metrica` escolhe medida (auto usa RMSE ou macro F1); `tentativas` define orçamento; `folds` define partições; `amplitude` escolhe limites conservadores/amplos; `estrategia` forma os folds: `aleatoria` (estratificada pela classe), `grupo` (cada grupo de `grupo` num só fold) ou `temporal` (origem móvel com janela crescente: os instantes de `ordem` formam `folds + 1` blocos contíguos e o fold i treina nos blocos 1 a i e valida no bloco i + 1, nunca no passado do treino); `seed` reproduz a busca. Também recebe `alvo`, `cols` e `tarefa`.

## Exemplo

```r
d <- trama.ml::tr_ml_example("mtcars")
z <- trama.ml::tr_ml_tune(d, alvo = "mpg", modelo = "cart", tentativas = 4, folds = 3)
z$modelo
head(z$historico)
```

## Como interpretar

A saída `modelo` é o vencedor reajustado; `historico` registra configurações, resultado por fold, média e melhor valor acumulado.
