---
section: colecoes
title: Dados para aprender
description: Fornece iris, iris binária ou mtcars como tabela reprodutível para explorar ajuste e avaliação.
collection: aprendizado
node: ml/example
related: ["ml/split", "ml/linear"]
---

## O que o bloco faz

Fornece iris, iris binária ou mtcars como tabela reprodutível para explorar ajuste e avaliação.

## Quando usar

Use iris para classificação em três classes, iris_binaria para duas espécies e mtcars para regressão. São conjuntos didáticos.

## Configuração

`nome`: `iris`, `iris_binaria` ou `mtcars`.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
str(d)
```

## Como interpretar

A tabela contém as observações e variáveis originais; iris_binaria mantém apenas duas espécies.
