---
section: colecoes
title: Aprendizado supervisionado
description: Prepare dados, ajuste modelos supervisionados, gere previsões e avalie seu desempenho.
collection: aprendizado
order: 1
related: [ml/example, ml/split, ml/predict]
---

A coleção `trama.ml` organiza um fluxo de aprendizado supervisionado: obtenha ou leia uma tabela, separe treino e teste, ajuste um modelo com o treino, aplique-o ao teste e interprete métricas e diagnósticos. As entradas de ajuste usam uma resposta e preditores numéricos; a resposta numérica indica regressão e uma resposta categórica indica classificação quando `tarefa = "auto"`.

## Fluxo de trabalho

`ml/example` fornece tabelas didáticas e `ml/split` reserva as linhas de teste. Os seis ajustes (`ml/linear`, `ml/cart`, `ml/figs`, `ml/forest`, `ml/svm` e `ml/xgboost`) produzem `ml/fit`; `ml/predict` acrescenta `.pred` e, quando disponíveis, probabilidades por classe. `ml/evaluate` resume desempenho e `ml/confusion` detalha acertos e confusões.

CART e FIGS também podem ser inspecionados com `ml/rules` e `ml/tree_plot`; CART, FIGS, floresta e XGBoost aceitam `ml/importance`. `ml/tune` conduz validação cruzada interna ao treino; `ml/tuning_plot` mostra o histórico. `ml/residuals` examina regressão e `ml/roc` examina discriminação binária.

## Exemplo completo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
s <- trama.ml::tr_ml_split(d, resposta = "Species", seed = 42)
m <- trama.ml::tr_ml_cart(s$treino, resposta = "Species", preditores = "Petal.Length, Petal.Width")
p <- trama.ml::tr_ml_predict(m, s$teste)
trama.ml::tr_ml_evaluate(p, resposta = "Species")
trama.ml::tr_ml_confusion(p, resposta = "Species")
```

O conjunto de teste é usado para a avaliação final. Escolha modelo e parâmetros apenas com treino ou validação interna; `ml/tune` usa folds nos dados recebidos e reajusta o vencedor nessas linhas. Os motores `rpart`, `figsr`, `ranger`, `e1071` e `xgboost` são dependências opcionais instaladas para os métodos correspondentes.

## Nós da coleção

- Dados e divisão: [Dados para aprender](/trama/colecoes/aprendizado/dados/, [Separar treino e teste](/trama/colecoes/aprendizado/separar-treino-teste/.
- Ajustes: [Linear / logística](/trama/colecoes/aprendizado/linear/, [CART](/trama/colecoes/aprendizado/cart/, [FIGS](/trama/colecoes/aprendizado/figs/, [Random forest](/trama/colecoes/aprendizado/forest/, [SVM](/trama/colecoes/aprendizado/svm/, [XGBoost](/trama/colecoes/aprendizado/xgboost/.
- Previsão e avaliação: [Prever](/trama/colecoes/aprendizado/predict/, [Avaliar previsões](/trama/colecoes/aprendizado/evaluate/, [Matriz de confusão](/trama/colecoes/aprendizado/confusion/.
- Inspeção: [Ler regras](/trama/colecoes/aprendizado/rules/, [Importância](/trama/colecoes/aprendizado/importance/, [Visualizar árvores](/trama/colecoes/aprendizado/tree-plot/, [Analisar resíduos](/trama/colecoes/aprendizado/residuals/, [Curva ROC](/trama/colecoes/aprendizado/roc/.
- Busca: [Ajustar hiperparâmetros](/trama/colecoes/aprendizado/tune/, [Visualizar tuning](/trama/colecoes/aprendizado/tuning-plot/.
