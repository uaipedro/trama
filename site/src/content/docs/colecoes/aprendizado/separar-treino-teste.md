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

`alvo` identifica a resposta; `proporcao` define a fração aproximada de treino; `estratificar` mantém classes nos dois lados; `seed` reproduz a amostra. `estrategia` respeita a dependência entre linhas: `aleatoria` (padrão) sorteia linhas; `temporal` põe no treino tudo até o instante da linha ⌊n · proporção⌋ na ordem de `ordem` (instantes empatados ficam juntos) e no teste o que vem depois; `grupo` sorteia ⌊G · proporção⌋ grupos inteiros de `grupo`, de modo que nenhum indivíduo, lote ou área aparece dos dois lados. Nas duas últimas, `estratificar` é ignorado.

## Exemplo

```r
d <- trama.ml::tr_ml_example("iris_binaria")
s <- trama.ml::tr_ml_split(d, alvo = "Species", proporcao = 0.75, seed = 42)
nrow(s$treino); nrow(s$teste)

# Dados com tempo: todo o teste é posterior ao treino.
e <- data.frame(dia = rep(1:20, each = 3), y = factor(rep(c("a", "b"), 30)), x = 1:60)
t <- trama.ml::tr_ml_split(e, alvo = "y", proporcao = 0.7, estrategia = "temporal", ordem = "dia")
range(t$treino$dia); range(t$teste$dia)   # 1-14 e 15-20
```

## Como interpretar

As saídas `treino` e `teste` não duplicam nem perdem linhas. A proporção final pode variar por arredondamento em cada classe.
