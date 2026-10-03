---
title: Reta ajustada
description: Acrescenta a um disperso a reta ou curva ajustada, com intervalo de confiança e equação com R².
section: colecoes
collection: visualizacao
node: view/fit_line
category: camadas
related: [view/points, view/reference, models/plot_regression]
---

## O que o bloco faz

`view/fit_line` recebe um Disperso (ou uma Linha) e acrescenta a reta ou curva ajustada aos próprios pontos do gráfico, com a faixa do intervalo de confiança e, na linear e na quadrática, a equação e o R² no canto. Os dados são o X e o Y do gráfico de entrada; não há tabela a ligar.

## Quando usar

Use para VER a tendência entre duas medidas e escrevê-la na figura, como nas teses. Para testar coeficientes, examinar resíduos e conferir pressupostos, ajuste o modelo na coleção de modelos; esta camada não faz inferência.

## Configuração

**Método** escolhe `linear`, `quadrática` (ŷ = a + b·x + c·x²) ou `loess` (regressão local, sem equação). **Intervalo** liga a faixa, e **Confiança (IC)** dá o nível, 0,95 por padrão. **Equação e R²** escreve a equação com vírgula decimal. Com painéis há um ajuste por painel; com cor por grupo e **Uma por cor** ligado, uma curva por grupo, na cor dele. Com eixo em log, o ajuste é na escala desenhada, e a equação diz `log₁₀(x)` ou `log₁₀(ŷ)`. **Posição da equação** em `automática` põe as equações, em cada painel, no canto com menos pontos (ou fixe um dos quatro cantos), e o eixo Y ganha espaço daquele lado para o texto não cobrir os pontos.

## Exemplo

```r
library(trama.view)
p <- tr_points(mtcars, x = "wt", y = "mpg", painel = "am")
tr_fit_line(p, metodo = "linear", confianca = 0.95)
```

## Como interpretar

A faixa é o intervalo de confiança da MÉDIA: mostra onde está a reta, e não onde cairá a próxima observação — o intervalo de predição é bem mais largo. O R² é a fração da variação de Y explicada pela curva naquele grupo; num grupo pequeno, ele varia muito de amostra para amostra.
