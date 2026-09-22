---
title: Pareto
description: Ordena categorias por contribuição e apresenta o percentual acumulado.
section: colecoes
collection: visualizacao
node: view/pareto
category: comparacao
related: [view/bars, view/dotplot, data/mutate]
---

## O que o bloco faz

`view/pareto` ordena categorias por contribuição e apresenta o percentual acumulado.

## Quando usar

Use para identificar quais categorias concentram a maior parte de ocorrências ou valores.

## Configuração

Categoria é obrigatória. Valor vazio conta linhas; preenchido soma e precisa ser não negativo. Referência (%) vai de 0 a 100; 0 remove a referência. A linha acumulada usa o total como 100%.

## Exemplo

```r
library(trama.view)
dados <- data.frame(defeito = c("trinca", "poro", "risco", "trinca", "poro", "trinca"),
                    custo = c(10, 5, 2, 8, 7, 6))
tr_pareto(dados, x = "defeito", y = "custo", referencia = 80)
```

## Como interpretar

As barras mostram contagem ou soma por categoria, em ordem decrescente. A linha acumulada indica que fração do total foi alcançada até cada categoria, e a linha de referência marca o percentual configurado. Categorias até esse cruzamento concentram a parcela indicada do total analisado.
