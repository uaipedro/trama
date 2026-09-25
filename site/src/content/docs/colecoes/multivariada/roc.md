---
title: Curva ROC
description: Compara sensibilidade e especificidade em cortes e resume discriminação pela AUC, com validação cruzada.
section: colecoes
collection: multivariada
node: multi/roc
related: [multi/logistic, multi/classify, multi/confusion]
---

## O que o bloco faz

O bloco `multi/roc` plota sensibilidade contra taxa de falsos positivos para vários cortes de probabilidade e calcula a AUC com intervalo de confiança de DeLong (DeLong, DeLong & Clarke-Pearson, 1988). Com três ou mais grupos, traça uma curva por grupo contra os outros e resume o classificador pela AUC multiclasse M de Hand & Till (2001) no subtítulo: a média, sobre os pares de grupos, da AUC de cada par, que não depende das proporções dos grupos. Na discriminante linear da `iris`, por validação cruzada, M = 0,998. O bloco recebe `multi/classifier`.

## Quando usar

Use **Curva ROC** para comparar a capacidade de ordenar positivos acima de negativos sem fixar previamente um corte.

## Configuração

- **Validação** — `cruzada` (padrão, deixa uma observação fora) ou `resubstituição`.
- **Confiança da AUC** — nível do intervalo de DeLong (padrão 0,95).
- **Aspecto**, **Tema**, **Título**, **Rótulo X**, **Rótulo Y** e **Legenda** — controlam a apresentação.

Em binária, o segundo nível é a classe positiva; não há parâmetro para alterá-la.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "pima") |>
  tr_add("log", "multi/logistic", grupo = "diabetes", cols = "glicose, imc, pedigree", from = "dados") |>
  tr_add("roc", "multi/roc", validacao = "cruzada", from = "log")
```

A curva usa probabilidades da validação cruzada; o segundo nível de `diabetes` é positivo.

## Como interpretar

Sensibilidade é a fração de positivos encontrados; eixo X é 1 − especificidade. AUC 0,5 corresponde à ordenação aleatória e 1 à separação perfeita. Em binária, positivo é o segundo nível; com três ou mais grupos, cada classe é comparada às demais. Na logística do `pima` com todos os preditores e validação cruzada, a AUC é 0,849 (IC 95% DeLong de 0,816 a 0,882). O intervalo é assintótico e trata as probabilidades de deixa-um-fora como um escore fixo.

## Veja também

- [`Regressão logística`](/trama/colecoes/multivariada/logistic/)
- [`Classificar`](/trama/colecoes/multivariada/classify/)
- [`Matriz de confusão`](/trama/colecoes/multivariada/confusion/)
