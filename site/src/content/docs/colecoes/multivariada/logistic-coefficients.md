---
title: Razões de chances
description: Apresenta coeficientes, erros padrão, p-valores e razões de chances com intervalo da verossimilhança perfilada (ou de Wald, como opção).
section: colecoes
collection: multivariada
node: multi/logistic_coefficients
related: [multi/logistic, multi/plot_odds, multi/roc]
---

## O que o bloco faz

O bloco `multi/logistic_coefficients` extrai coeficientes, erros padrão, testes, razões de chances e intervalos de confiança em uma tabela. Por padrão (versão 4) o intervalo é o da verossimilhança perfilada e o p é o da razão de verossimilhanças — na logística por máxima verossimilhança binária (Venables & Ripley, 2002; Hosmer, Lemeshow & Sturdivant, 2013, que o preferem em amostra pequena) e, com a verossimilhança penalizada, na de Firth (`metodo = "firth"`; Heinze & Schemper, 2002). Na multinomial o intervalo e o p continuam de Wald, por falta de implementação de referência do perfil. A coluna `intervalo` diz qual foi calculado. O bloco recebe `multi/logit`.

## Quando usar

Use **Razões de chances** para quantificar direção e tamanho dos efeitos estimados pelos preditores.

## Configuração

- **Escala** — `unidade` (padrão) ou `desvio padrão`, para expressar o efeito por desvio padrão da variável.
- **Confiança do intervalo** (`confianca`) — 0,95 por padrão, entre 0,5 e 0,999. Até a versão 2 do bloco o param se chamava `nivel`: fluxo salvo com `nivel` acusa param desconhecido ao abrir e precisa renomeá-lo.
- **Intervalo** — `perfilado` (padrão) ou `Wald` (o padrão até a versão 3). Na multinomial, sempre Wald.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "pima") |>
  tr_add("log", "multi/logistic", grupo = "diabetes", cols = "glicose, imc, pedigree", from = "dados") |>
  tr_add("coef", "multi/logistic_coefficients", from = "log")
```

A tabela resume coeficientes e razões de chances da logística ajustada. No `pima`, a razão de chances do `pedigree` é 3,71, com IC 95% perfilado de 1,88 a 7,45 (o de Wald daria 1,86 a 7,39): com 768 casos os dois quase coincidem; a diferença cresce com poucos eventos por preditor.

## Como interpretar

Coeficiente está em log-chances; razão de chances é `exp(coeficiente)`. Valor 1 indica ausência de mudança na chance por unidade. Intervalo que inclui 1 corresponde a coeficiente compatível com 0.

## Veja também

- [`Regressão logística`](/trama/colecoes/multivariada/logistic/)
- [`Gráfico das razões de chances`](/trama/colecoes/multivariada/plot-odds/)
- [`Curva ROC`](/trama/colecoes/multivariada/roc/)
