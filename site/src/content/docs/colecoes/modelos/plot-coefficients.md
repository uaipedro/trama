---
title: Gráfico dos coeficientes
description: "Gráfico de floresta: cada coeficiente com o intervalo de confiança, contra a linha do zero (ou do 1)."
section: colecoes
collection: modelos
node: models/plot_coefficients
category: resumir
related: [models/coefficients, models/glm, models/effect_size]
---

## O que o bloco faz

`models/plot_coefficients` desenha os coeficientes do modelo em gráfico de floresta: um ponto por termo na estimativa, a barra no intervalo de confiança e uma linha tracejada na referência (0, ou 1 quando exponenciado). O intercepto fica de fora. A saída é um gráfico.

## Quando usar

Para mostrar num relance quais preditores têm efeito e em que direção. Funciona com todo modelo que tem coeficientes: regressão linear, GLM, misto, a logística da coleção multivariada e a regressão linear da coleção de aprendizado de máquina. Substitui o antigo gráfico das razões de chances da multivariada; fluxos antigos abrem aqui já exponenciados.

## Configuração

Exponenciar (GLM de ligação log ou logit) mostra razões de chances ou de taxas em eixo log. Escala `desvio padrão` põe as preditoras em unidades comparáveis. Confiança define o nível do intervalo (padrão 0,95). Ordenar deixa os termos na ordem do modelo ou do maior para o menor.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "mtcars") |>
  tr_add("logit", "models/glm", formula = "am ~ wt + hp", familia = "binomial", from = "dados") |>
  tr_add("floresta", "models/plot_coefficients", exponenciar = TRUE, escala = "desvio padrão", from = "logit")
```

Na logística do câmbio manual (`am`) em função do peso e da potência, por desvio padrão, o peso reduz fortemente a chance de câmbio manual e a potência a aumenta.

## Como interpretar

A cor diz de que lado da referência está o intervalo: acima, abaixo, ou cruzando (cinza, efeito não distinguível de zero ao nível escolhido). Barras comparam-se entre si só na escala por desvio padrão; por unidade, dependem da unidade de cada preditora.
