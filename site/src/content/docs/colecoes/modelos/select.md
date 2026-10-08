---
title: Selecionar modelos
description: "Ranqueia vários modelos da mesma resposta por AICc, AIC ou BIC, com diferença para o melhor, pesos de Akaike e razão de evidência."
section: colecoes
collection: modelos
node: models/select
category: resumir
related: [models/compare, models/fit_stats]
---

## O que o bloco faz

`models/select` ranqueia dois ou mais modelos da mesma resposta por um critério de informação, numa única tabela. Diferente do `models/compare`, os modelos não precisam ser aninhados: liga-se um cabo por modelo.

A tabela traz, do melhor ao pior, o valor do critério, a diferença para o melhor (`delta`), o peso de Akaike (`peso`) e a razão de evidência (`razao_evidencia`), que é o peso do melhor dividido pelo do modelo.

## Quando usar

Quando há alguns modelos candidatos para os mesmos dados, por exemplo com e sem um termo de interação, ou com formas diferentes de tendência, e se quer saber qual deles o conjunto sustenta melhor. Para dois modelos aninhados, com um teste formal, o bloco indicado é o `models/compare`.

## Configuração

**Critério**: `AICc` (o padrão, que corrige o AIC para amostra pequena e converge para ele com n grande), `AIC` ou `BIC`.

Os modelos precisam ter a mesma resposta, escrita da mesma forma (`log(y)` e `y` não se comparam), e as mesmas linhas. Modelos mistos e GLS são reajustados por máxima verossimilhança antes da comparação. O bloco não aceita família quasi, que não tem verossimilhança, nem mistura resposta discreta (binomial, Poisson) com contínua (gaussiana, gama).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("carros", "models/example", dataset = "mtcars") |>
  tr_add("m1", "models/lm", formula = "mpg ~ wt", from = "carros") |>
  tr_add("m2", "models/lm", formula = "mpg ~ wt + hp", from = "carros") |>
  tr_add("m3", "models/lm", formula = "mpg ~ wt * hp", from = "carros") |>
  tr_add("ranking", "models/select", from = c("m1", "m2", "m3"))
```

## Como interpretar

Pela regra de Burnham e Anderson (2002), um `delta` de até 2 indica suporte substancial; de 4 a 7, bem menos; acima de 10, praticamente nenhum. A transição entre as faixas é gradual, e a regra serve para triagem, não como teste. O peso de Akaike é a probabilidade relativa de o modelo ser o melhor dentro do conjunto comparado, e muda se o conjunto mudar. O ranking diz qual modelo o conjunto sustenta, não se algum deles é bom: os pressupostos do modelo escolhido continuam precisando ser conferidos. Com n − k − 1 ≤ 0 o AICc não existe, e o modelo vai para o fim da tabela, sem peso.
