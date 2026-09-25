---
section: colecoes
title: Validação cruzada aninhada
description: Estima o desempenho do ajuste com busca de hiperparâmetros sem reaproveitar as linhas que escolheram o vencedor.
collection: aprendizado
node: ml/nested_cv
related: ["ml/tune", "ml/split", "ml/evaluate"]
---

## O que o bloco faz

Cada fold externo roda um `ml/tune` completo só no seu treino e mede o vencedor na sua validação, que a busca nunca viu. A média da coluna `externa` estima o desempenho do procedimento inteiro — busca incluída.

## Quando usar

Quando não há linhas para um teste separado, ou quando se quer saber quanto da média do `ml/tune` é sorte da seleção. A média dos folds do vencedor é otimista porque ele foi o melhor entre muitas tentativas (Varma & Simon 2006); em ruído puro, com 40 linhas e SVM com 10 tentativas, a média do `ml/tune` deu 0,60 de acurácia e a aninhada 0,52 (acaso = 0,5), em 12 réplicas semeadas do teste do bloco.

## Configuração

Os mesmos parâmetros do `ml/tune` (`modelo`, `metrica`, `tentativas`, `amplitude`, `estrategia`, `ordem`, `grupo`, `seed`), mais `folds_externos`; `folds` são as partições internas de cada busca. O custo é folds externos × tentativas × folds internos ajustes.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
trama.ml::tr_ml_nested_cv(d, alvo = "Species", tentativas = 5, folds_externos = 5, folds = 3)
```

## Como interpretar

Uma linha por fold externo e a linha `media`. No exemplo, a `interna` média é 0,904 e a `externa` 0,890 (macro F1): a diferença é o otimismo da seleção. Reporte a `externa`; o modelo a usar sai de um `ml/tune` em todas as linhas.
