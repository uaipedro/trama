---
section: colecoes
title: Separar treino e teste
description: Divide as linhas em duas tabelas reprodutivelmente e pode preservar a proporção de cada classe.
collection: aprendizado
node: ml/split
related: ["ml/cart", "ml/predict"]
---

## O que o bloco faz

Divide as linhas em duas tabelas reprodutivelmente e pode preservar a proporção de cada classe.

## Quando usar

Reserve um conjunto de teste antes do ajuste para estimar desempenho em linhas não usadas no treino.

## Configuração

`resposta` identifica a resposta; `proporcao` define a fração aproximada de treino; `estratificar` mantém classes nos dois lados; `seed` reproduz a amostra. Para séries temporais ou grupos dependentes, a separação precisa respeitar essa estrutura.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
s <- trama.ml::tr_ml_split(d, resposta = "Species", proporcao = 0.75, seed = 42)
nrow(s$treino); nrow(s$teste)
```

## Como interpretar

As saídas `treino` e `teste` não duplicam nem perdem linhas. A proporção final pode variar por arredondamento em cada classe.
