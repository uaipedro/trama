---
title: Regressão de doses
description: "Desdobra o tratamento quantitativo da ANOVA em linear, quadrático e cúbico, e ajusta a curva."
section: colecoes
collection: modelos
node: models/dose_response
category: anova
related: [models/anova_dbc, models/plot_regression, models/coefficients]
---

## O que o bloco faz

`models/dose_response` recebe o modelo de uma ANOVA (`models/anova_dic`, `models/anova_dbc` ou `models/anova_dql`) em que o tratamento é uma dose, desdobra os graus de liberdade do tratamento em componentes linear, quadrático e cúbico, testa cada um contra o resíduo da ANOVA e ajusta a curva do grau escolhido. Tem duas saídas: `modelo` (`models/fit`, a curva) e `quadro` (`models/effects`, o desdobramento).

## Quando usar

Use quando o tratamento é quantitativo — doses de adubo, lâminas de irrigação, densidades — e a pergunta é como a resposta muda com a dose, e não apenas se as doses diferem. É a análise de regressão que acompanha a ANOVA nas teses de ciências agrárias.

## Configuração

Tratamento é o fator de doses do modelo; os níveis precisam ser números. Grau `automático` escolhe o maior componente significativo (até o cúbico) ao nível 1 − Confiança; 1, 2 ou 3 fixam o grau. O quadro traz a falta de ajuste do grau escolhido. A curva é ajustada às médias das doses, com o erro da ANOVA nos erros padrão, e o R² é SQ da regressão / SQ de tratamentos. Na parábola sai a dose de máxima eficiência técnica, −b₁ / (2 b₂).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "adubo_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "dose", bloco = "bloco", from = "dados") |>
  tr_add("resultado", "models/dose_response", tratamento = "dose", from = "ajuste")
```

`adubo_dbc` simula cinco doses de nitrogênio com resposta quadrática. O desdobramento separa linear e quadrático significativos, cúbico e desvios não significativos, e o grau automático é 2, com R² perto de 1 e a dose de máxima produção entre 100 e 200 kg/ha.

## Como interpretar

Componente significativo quer dizer que aquela forma explica parte da diferença entre as doses. A falta de ajuste significativa avisa que a curva escolhida deixa diferença sem explicar. A dose de máxima eficiência técnica é a de maior resposta prevista; quando cai fora das doses testadas, a nota avisa que é extrapolação.
