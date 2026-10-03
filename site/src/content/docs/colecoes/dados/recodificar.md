---
title: Recodificar
description: Troque valores de uma coluna ou fatie um número em faixas.
section: colecoes
collection: dados
node: data/recode
category: transformar
order: 25
related: [data/mutate, data/convert]
---

## O que o bloco faz

Dois modos, um OU outro. **Níveis** (`niveis`): pares `de=para` separados por `;` — valor citado que não existe é erro, fator continua fator. **Cortes** (`cortes`): limites de faixas separados por `;`, fechadas à direita `(a,b]` ou à esquerda `[a,b)` (`fechado`), com **Rótulos** (`rotulos`) opcionais, um por faixa. À esquerda, a última faixa fecha nos dois lados (o maior corte entra nela); à direita, a primeira. Coluna numérica recodificada por níveis vira texto. Valor fora dos cortes para o bloco (use `-Inf`/`Inf`). **Coluna de saída** (`nome`) em branco substitui a original.

## Exemplo

```r
tr_add("faixa", "data/recode", variavel = "idade", cortes = "0; 18; 60; Inf", rotulos = "jovem; adulto; idoso", fechado = "esquerda", nome = "faixa", from = "ler")
```
