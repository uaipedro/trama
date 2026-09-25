---
section: colecoes
title: Matriz de confusão
description: Conta combinações entre classe observada e prevista e devolve colunas `observado`, `previsto` e `n`.
collection: aprendizado
node: ml/confusion
related: ["ml/predict", "ml/evaluate"]
---

## O que o bloco faz

Conta combinações entre classe observada e prevista e devolve colunas `observado`, `previsto` e `n`.

## Quando usar

Use para localizar quais classes se confundem e interpretar métricas agregadas de classificação.

## Configuração

`alvo` nomeia a classe observada e `predito` a classe prevista (padrão `.pred`).

## Exemplo

```r
d <- data.frame(y = c("a", "a", "b"), .pred = c("a", "b", "b"))
trama.ml::tr_ml_confusion(d, alvo = "y")
```

## Como interpretar

Cada linha é um par de classes e sua contagem; combinações sem ocorrências também aparecem. Leia as contagens junto ao tamanho de cada classe.

### Previsões do treino

Se as linhas vierem do `treino` marcado pelo [`ml/split`](/trama/colecoes/aprendizado/separar-treino-teste/), o bloco recusa por padrão (`tr_ml_error_train_eval`): a avaliação no treino é otimista e não mede generalização. Para medir o ajuste no treino de propósito (por exemplo, comparar com o teste e ver o sobreajuste), ligue `permitir_treino`; o resultado vem com um aviso e a nota de otimismo. O mesmo vale para uma tabela marcada como teste que traz linhas de fora dele (previsões do treino juntadas às do teste). Tabelas sem a marca do `ml/split` são avaliadas como chegam.
