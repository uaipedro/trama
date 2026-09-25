---
section: colecoes
title: Importância de variáveis
description: Ordena a importância interna das variáveis em modelos de árvore.
collection: aprendizado
node: ml/importance
related: ["ml/cart", "ml/figs", "ml/forest", "ml/xgboost", "ml/rules"]
---

## O que o bloco faz

Ordena a importância interna das variáveis em modelos de árvore.

## Quando usar

Use para resumir quais preditores participaram mais do ajuste em CART, FIGS, floresta ou XGBoost.

## Configuração

Recebe `modelo` ajustado por CART, FIGS, random forest ou XGBoost. Na random forest, a medida é a escolhida em `importancia` no `ml/forest`: redução de impureza (padrão), permutação (aumento do erro fora da bolsa ao embaralhar o preditor; Breiman 2001 — na classificação é o erro de Brier do `ranger`, média de (1 − p da classe observada)², não a queda de acurácia; na regressão, o erro quadrático médio) ou impureza corrigida (AIR; Nembrini, König & Wright 2018). No XGBoost, o número é o Gain relativo (fração do ganho total das divisões, soma 1); no CART, a redução de impureza inclui as divisões substitutas do `rpart`. A saída traz a coluna `medida`, que diz em palavras o que cada número mede.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
m <- trama.ml::tr_ml_cart(d, "Species", "Petal.Length, Petal.Width")
trama.ml::tr_ml_importance(m)
```

## Como interpretar

A tabela contém `variavel` e `importancia`. As medidas dependem do motor e não são comparáveis entre famílias; variáveis correlacionadas podem repartir importância.

A redução de impureza favorece preditores contínuos ou com muitos valores distintos (Strobl et al. 2007). Numa floresta de 150 árvores em 120 linhas com `y = 2·x1 + erro`, um `x2` com 3 valores e um `ruido` contínuo sem relação com `y` (semente 11), a impureza dá ao `ruido` 54,9 contra 282,5 do `x1` e 10,0 do `x2`; a permutação dá 4,44 ao `x1` e valores perto de zero (−0,02 e −0,21) aos outros dois, e a impureza corrigida dá 120,8 ao `x1` e −0,43 e −1,91 aos outros. Valores negativos indicam preditor sem informação.
