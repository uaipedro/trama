---
title: Classificar
description: Aplica um classificador a dados de treino ou a uma tabela nova e retorna classe prevista e probabilidades.
section: colecoes
collection: multivariada
node: multi/classify
related: [multi/discriminant, multi/logistic, multi/confusion]
---

## O que o bloco faz

O bloco `multi/classify` aplica LDA ou logística a observações e retorna grupo previsto, probabilidades por grupo e escores LD quando disponíveis. O bloco recebe `multi/classifier` e aceita a tabela opcional `novos`.

## Quando usar

Use **Classificar** para gerar previsões no treino ou classificar uma tabela nova que ainda não tem o grupo conhecido.

## Configuração

**Validação** — `resubstituição` (padrão) ou `cruzada`. **Modelo** recebe a saída classificadora. A porta opcional **Novos** recebe a tabela a classificar; validação cruzada vale quando Novos não está conectado.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", grupo = "Species", from = "dados") |>
  tr_add("cl", "multi/classify", from = "lda")
```

A tabela resultante acrescenta grupo previsto e probabilidades às observações de `iris`.

## Como interpretar

`previsto` é o grupo atribuído; `prob_<grupo>` mostra a probabilidade estimada. Probabilidades próximas indicam decisão incerta. A tabela nova precisa conter, com os mesmos nomes, todos os preditores do ajuste.

## Veja também

- [`Discriminante`](/trama/colecoes/multivariada/discriminant/)
- [`Regressão logística`](/trama/colecoes/multivariada/logistic/)
- [`Matriz de confusão`](/trama/colecoes/multivariada/confusion/)
