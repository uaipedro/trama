---
title: Multivariada
description: Explore correlações, reduza dimensões, estime fatores e classifique grupos com saídas encadeáveis.
section: colecoes
collection: multivariada
related: [multi/example, multi/pca]
---

## Organização da coleção

`trama.multi` recebe `data/table` e conecta tabelas a modelos ou gráficos. Um fluxo pode conferir fatorabilidade com KMO e Bartlett, estimar a quantidade de componentes por análise paralela e então ajustar PCA ou análise fatorial. Para classificar grupos, a coleção oferece LDA/QDA e logística; os dois saem como modelo (`models/fit`), e prever, validar e avaliar é com os blocos da coleção de modelos (`models/predict`, `models/confusion`, `models/roc`).

## Fluxo reproduzível

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "USArrests") |>
  tr_add("pca", "multi/pca", cols = "Murder, Assault, UrbanPop, Rape",
         padronizar = TRUE, from = "dados") |>
  tr_add("variancia", "multi/pca_variance", from = "pca") |>
  tr_add("mapa", "multi/biplot", from = "pca")
```

A coleção contém conjuntos reais do R e conjuntos simulados com estruturas conhecidas. `USArrests` evidencia o efeito da padronização; `questionario` permite comparar cargas fatoriais plantadas e estimadas; `iris` serve à classificação discriminante; `pima` demonstra logística binária e ROC.

## Blocos

- [Exemplo multivariado](/trama/colecoes/multivariada/example/) e [KMO e Bartlett](/trama/colecoes/multivariada/kmo-bartlett/), [Análise paralela](/trama/colecoes/multivariada/parallel/)
- Correlação: [Matriz](/trama/colecoes/multivariada/correlation-matrix/) e [Mapa](/trama/colecoes/multivariada/plot-correlation/)
- PCA: [Componentes principais](/trama/colecoes/multivariada/pca/), [Variância explicada](/trama/colecoes/multivariada/pca-variance/), [Cargas](/trama/colecoes/multivariada/pca-loadings/), [Scree](/trama/colecoes/multivariada/scree/), [Biplot](/trama/colecoes/multivariada/biplot/), [Círculo de correlações](/trama/colecoes/multivariada/correlation-circle/), [Jackknife](/trama/colecoes/multivariada/jackknife-pca/)
- Fatorial: [Análise fatorial](/trama/colecoes/multivariada/factor-analysis/), [Cargas](/trama/colecoes/multivariada/fa-loadings/), [Mapa das cargas](/trama/colecoes/multivariada/plot-loadings/), [Jackknife](/trama/colecoes/multivariada/jackknife-fa/)
- Classificação: [Discriminante](/trama/colecoes/multivariada/discriminant/), [Prever](/trama/colecoes/modelos/predict/) e [Matriz de confusão](/trama/colecoes/modelos/confusion/) (da coleção de modelos), [Funções discriminantes](/trama/colecoes/multivariada/discriminant-functions/), [M de Box](/trama/colecoes/multivariada/box-m/), [Plano discriminante](/trama/colecoes/multivariada/plot-discriminant/), [Jackknife](/trama/colecoes/multivariada/jackknife-discriminant/)
- Logística: [Regressão](/trama/colecoes/multivariada/logistic/), [Coeficientes](/trama/colecoes/modelos/coefficients/) (modelos), [Gráfico das razões](/trama/colecoes/multivariada/plot-odds/), [Curva ROC](/trama/colecoes/modelos/roc/) (modelos), [Jackknife](/trama/colecoes/multivariada/jackknife-logistic/)
