---
section: colecoes
title: Ajustar hiperparâmetros
description: Avalia configurações por validação cruzada nos dados recebidos e reajusta a melhor configuração em todas as linhas.
collection: aprendizado
node: ml/tune
related: ["ml/split", "models/predict", "models/evaluate", "ml/tuning_plot"]
---

## O que o bloco faz

Avalia configurações por validação cruzada nos dados recebidos e reajusta a melhor configuração em todas as linhas.

## Quando usar

Conecte apenas treino. Mantenha o teste separado até a avaliação final.

## Configuração

`modelo` aceita CART, FIGS, forest, SVM ou XGBoost; `metrica` escolhe medida (auto usa RMSE ou macro F1); `tentativas` define orçamento; `folds` define partições; `amplitude` escolhe limites conservadores/amplos; com CART cada ajuste inclui a poda 1-EP, com validação cruzada interna própria (até 10 ajustes extras por fold), e `cp` fica fora da busca; `estrategia` forma os folds: `aleatoria` (estratificada pela classe), `grupo` (cada grupo de `grupo` num só fold) ou `temporal` (origem móvel com janela crescente: os instantes de `ordem` formam `folds + 1` blocos contíguos e o fold i treina nos blocos 1 a i e valida no bloco i + 1, nunca no passado do treino); `seed` reproduz a busca. Também recebe `resposta`, `preditores` e `tarefa`.

Na classificação, se algum fold de validação tiver uma classe só (comum com `grupo` quando cada grupo tem uma classe), macro F1, kappa e acurácia balanceada degeneram nele: o bloco avisa e guarda a nota em `nota`. A saída é trocar para `grupo_estratificado` ou usar menos folds.

## Exemplo

```r
d <- trama.ml::tr_ml_example("mtcars")
z <- trama.ml::tr_ml_tune(d, resposta = "mpg", modelo = "cart", tentativas = 4, folds = 3)
z$modelo
head(z$historico)
```

## Como interpretar

A saída `modelo` é o vencedor reajustado; `historico` registra configurações, resultado por fold, média e melhor valor acumulado.

### Teste recusado

A saída `teste` do [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/) é recusada aqui (`tr_ml_error_test_leak`): ajustar nela treinaria no teste. Ligue a saída `treino`; o teste vai só ao `ml/predict`.
