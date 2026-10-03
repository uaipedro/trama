---
title: Linha de referência
description: Acrescenta a um gráfico linhas de referência (limite, meta, y = x) e uma faixa sombreada.
section: colecoes
collection: visualizacao
node: view/reference
category: camadas
related: [view/fit_line, view/annotate, view/points]
---

## O que o bloco faz

`view/reference` recebe um gráfico e devolve o mesmo gráfico com uma ou mais linhas de referência: horizontal (y = valor), vertical (x = valor) ou diagonal (y = valor + inclinação·x). Com **Faixa até**, sombreia a faixa entre o valor e esse número. O tema, a proporção e os rótulos continuam os do gráfico de entrada.

## Quando usar

Use para mostrar o que o leitor compara com os pontos: o limite legal, a meta, a média histórica, a reta y = x de um observado contra previsto, a faixa aceitável de um controle de qualidade.

## Configuração

**Valor** aceita um número ou vários separados por `;` (`10; 20`), com vírgula decimal (`2,5`). **Texto** escreve um rótulo junto da linha; vários textos, separados por `;`, vão um para cada valor. A faixa pede um valor só e não vale na diagonal. **Estilo** escolhe entre tracejada, contínua e pontilhada.

Com painéis, a referência aparece em todos. Com eixo em log, o valor é dado na unidade dos dados; a diagonal é recusada em eixo log, porque ali a reta seria outra curva. Um painel (`view/combine`) não é aceito: ligue a referência no gráfico, antes do painel.

## Exemplo

```r
library(trama.view)
p <- tr_points(mtcars, x = "wt", y = "mpg")
tr_reference(p, "horizontal", valor = "20", ate = "25", texto = "faixa-alvo")
```

## Como interpretar

A linha é um valor fixo que você escolheu, e não uma estimativa. Diga no texto do gráfico ou na legenda da figura de onde ela vem (norma, meta, média de outro período).
