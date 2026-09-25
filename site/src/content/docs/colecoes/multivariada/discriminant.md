---
title: Discriminante
description: Ajusta LDA ou QDA usando grupos conhecidos para classificar observações por preditores.
section: colecoes
collection: multivariada
node: multi/discriminant
related: [multi/classify, multi/confusion, multi/box_m]
---

## O que o bloco faz

O bloco `multi/discriminant` ajusta LDA ou QDA com grupo conhecido e preditores numéricos e devolve um classificador. O bloco recebe `data/table`.

## Quando usar

Use **Discriminante** quando os grupos são conhecidos no treino e a tarefa é classificar casos por medidas observadas.

## Configuração

- **Resposta** — coluna da classe conhecida.
- **Preditores** — colunas numéricas; em branco, usa as numéricas menos a Resposta.
- **Método** — `linear` (padrão, LDA) ou `quadrática` (QDA).
- **Priors** — `proporcionais` (padrão) às frequências ou `iguais`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", resposta = "Species", preditores = "Petal.Length, Petal.Width", from = "dados")
```

O classificador LDA pode ser conectado à classificação e avaliação cruzada.

## Como interpretar

LDA usa covariância comum aos grupos; QDA estima uma covariância por grupo. A taxa do treino é aparente; `multi/confusion` em validação cruzada estima o desempenho em casos não usados no ajuste.

## Veja também

- [`Classificar`](/trama/colecoes/multivariada/classify/)
- [`Matriz de confusão`](/trama/colecoes/multivariada/confusion/)
- [`M de Box`](/trama/colecoes/multivariada/box-m/)
