---
title: Regressão polinomial
description: "Desdobra o tratamento quantitativo da ANOVA em linear, quadrático, cúbico... e ajusta a curva."
section: colecoes
collection: modelos
node: models/polinomial
category: medias
related: [models/anova_dbc, models/plot_regression, models/coefficients]
---

## O que o bloco faz

`models/polinomial` recebe o modelo de uma ANOVA (`models/anova_dic`, `models/anova_dbc` ou `models/anova_dql`) em que o tratamento é uma dose, desdobra a soma de quadrados de tratamentos em componentes linear, quadrático, cúbico... (1 gl cada, testados com o QM do resíduo da ANOVA) e ajusta a curva do grau escolhido. Tem duas saídas: `modelo` (`models/fit`, a curva, que vai para `models/coefficients`, `models/predict` e `models/plot_regression`) e `quadro` (`models/effects`, o desdobramento, com a equação, o R² e a dose de máxima eficiência técnica no rodapé).

Desde a versão 2 ele reúne o antigo `models/dose_response`; fluxos gravados com esse id abrem aqui.

## Quando usar

Quando o tratamento é quantitativo — doses de adubo, lâminas de irrigação, densidades, épocas — e a pergunta é como a resposta muda com a dose, e não só se as doses diferem. Com níveis igualmente espaçados e repetições iguais, a decomposição é a dos polinômios ortogonais dos livros; com espaçamento desigual entram as doses reais, e com repetições desiguais ou parcela perdida a partição é sequencial (cada grau depois do bloco e dos graus menores).

## Configuração

Tratamento é o fator de doses (níveis numéricos; vírgula decimal serve). Grau da curva `automático` escolhe o maior componente significativo a 1 − Confiança; se nenhum é, a curva é a média geral (grau 0) e a nota diz isso. De 1 a 5 fixam o grau. Maior grau testado (padrão 3) é quantos componentes o quadro testa; desce sozinho até k − 1 com poucas doses. O quadro traz os desvios da regressão e a falta de ajuste do grau escolhido.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "adubo_dbc") |>
  tr_add("ajuste", "models/anova_dbc", resposta = "producao", tratamento = "dose", bloco = "bloco", from = "dados") |>
  tr_add("resultado", "models/polinomial", tratamento = "dose", from = "ajuste")
```

`adubo_dbc` simula cinco doses de nitrogênio (0 a 200 kg/ha) em quatro blocos. O linear (F = 228,2) e o quadrático (F = 104,1) são significativos, o cúbico e os desvios não (p = 0,66 e 0,71), e o grau automático é 2: ŷ = 3,051 + 0,0363x − 0,0001262x², R² = 0,9989, falta de ajuste do grau 2 com p = 0,84 e dose de máxima produção 143,8 kg/ha.

## Como interpretar

Componente significativo quer dizer que aquela forma explica parte da diferença entre as doses. A falta de ajuste significativa avisa que a curva escolhida deixa diferença sem explicar. O R² é SQ da regressão / SQ de tratamentos: com três doses e grau 2 a curva passa por todas as médias e o R² é 1 por construção. A equação vale só dentro da faixa testada; a dose de máxima eficiência técnica, −b₁ / (2 b₂), quando cai fora das doses, é extrapolação e a nota avisa. A validação reproduz as SQ do exemplo do algodão de Montgomery (33,62, 343,21, 64,98 e 33,95), os contrastes `contr.poly` com as doses reais como scores e a ANOVA sequencial de `lm(y ~ bloco + x + x² + x³ + dose)` no DBC desbalanceado. A validação usa os dados do algodão de Montgomery (*Design and Analysis of Experiments*, 5.ª ed., tabela 3.1): as SQ 33,62, 343,21, 64,98 e 33,95 são calculadas desses dados e conferidas à mão pelos contrastes ortogonais (não copiadas de página impressa); e a decomposição sequencial do `lm` com `poly()`.
