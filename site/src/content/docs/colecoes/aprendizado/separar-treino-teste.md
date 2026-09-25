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

### Proveniência: o teste fica isolado por construção

As duas saídas levam uma marca (atributo `tr_ml_origem`, com o papel `treino`/`teste` e um id da divisão) que sobrevive ao cache e a filtros ou colunas novas. Com ela:

- ajustar um modelo, o `ml/tune` ou o `ml/nested_cv` na saída `teste` é recusado (`tr_ml_error_test_leak`);
- `ml/evaluate`, `ml/confusion`, `ml/roc` e `ml/pr_curve` recusam previsões das linhas de `treino` (`tr_ml_error_train_eval`), a menos que se ligue `permitir_treino` — aí o resultado sai com a nota “avaliação no treino é otimista”;
- o `ml/predict` recusa o teste de outra divisão com um modelo ajustado no treino desta (`tr_ml_error_split_mismatch`).

Tabelas sem a marca — divisão feita por fora do `ml/split`, ou treino e teste juntados — seguem como antes: o bloco não tem como saber de onde vieram as linhas, e o isolamento fica com você.
