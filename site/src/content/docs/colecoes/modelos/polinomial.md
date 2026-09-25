---
title: Regressão polinomial
description: "Regressão nos tratamentos quantitativos: SQ por grau, falta de ajuste, equação e R²."
section: colecoes
collection: modelos
node: models/polinomial
category: medias
related: [models/anova_dic, models/anova_dbc, models/anova_table]
---

## O que o bloco faz

`models/polinomial` decompõe a soma de quadrados de tratamentos de uma ANOVA (DIC ou DBC) em graus de um polinômio nos níveis do tratamento: linear, quadrático, cúbico... Cada grau tem 1 gl e é testado com o QM do resíduo da ANOVA. O que sobra até k − 1 gl é a falta de ajuste. A saída é `models/effects`, com a equação do maior grau significativo e o R² no rodapé.

## Quando usar

Quando os tratamentos são quantitativos (doses, épocas, espaçamentos). Comparar essas médias duas a duas ignora a ordem dos níveis; a regressão descreve a resposta ao longo da faixa. Com níveis igualmente espaçados e repetições iguais, a decomposição é a dos polinômios ortogonais dos livros. Com espaçamento ou repetições desiguais, é a mesma decomposição sequencial: o acréscimo de SQ de cada grau sobre os menores.

## Configuração

Informe o tratamento (com níveis numéricos), o maior grau a testar (até 5 e menor que o número de níveis) e o nível usado para escolher o grau da equação.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "ToothGrowth") |>
  tr_add("dic", "models/anova_dic", resposta = "len", tratamento = "dose", from = "dados") |>
  tr_add("reg", "models/polinomial", tratamento = "dose", grau = 2, from = "dic")
```

No `ToothGrowth`, as três doses de vitamina C (0,5, 1 e 2 mg) têm SQ linear 2224,3 (F = 123,6) e quadrática 202,1 (F = 11,2, p = 0,0014), com QM do resíduo 18,0 e 57 gl. A equação é ŷ = −2,49 + 30,15x − 7,93x². O R² é 100% porque, com três níveis, o polinômio de grau 2 passa por todas as médias: com poucos níveis, o R² diz pouco.

## Como interpretar

Fique com o maior grau significativo cuja falta de ajuste não seja significativa. A equação vale só dentro da faixa testada. A validação usa os dados do algodão de Montgomery (*Design and Analysis of Experiments*, 5.ª ed., tabela 3.1): as SQ 33,62, 343,21, 64,98 e 33,95 são calculadas desses dados e conferidas à mão pelos contrastes ortogonais (não copiadas de página impressa); e a decomposição sequencial do `lm` com `poly()`.
