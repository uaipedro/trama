---
title: Previsão de referência
description: "Média, ingênuo, ingênuo sazonal ou deriva: o que qualquer modelo tem de bater."
section: colecoes
collection: series-temporais
node: series/baseline
category: Modelar
order: 2
related: [series/accuracy, series/forecast]
---

## O que o bloco faz

Previsões que não estimam nada, e é por isso que servem: são a régua. Um
ARIMA que erra mais que "o mesmo mês do ano passado" não merece o card — e sem
a referência ao lado ninguém fica sabendo.

- **média** — todo futuro é a média da série inteira.
- **ingênuo** — todo futuro é o último valor observado. Imbatível em passeio
  aleatório (cotações, câmbio).
- **ingênuo sazonal** — cada mês futuro é o mesmo mês do último ano. A
  referência certa para série sazonal; pede ciclo.
- **deriva** — o último valor mais a inclinação média da série: uma reta do
  primeiro ao último ponto, prolongada.

Os intervalos saem como os de um modelo, e o `series/accuracy` os compara nos
mesmos termos.

## Quando usar

Estabeleça uma previsão de referência antes de avaliar modelos mais elaborados. Um modelo só acrescenta valor se superar essa regra simples no período de teste.

## Configuração

- **Método** — `média`, `ingênuo`, `ingênuo sazonal` ou `deriva`.
- **Horizonte** — quantos períodos prever.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("treino", "series/window", fim = "1958", from = "pax") |>
  tr_add("ref", "series/baseline", metodo = "ingênuo sazonal", horizonte = 24L,
         from = "treino") |>
  tr_add("erro", "series/accuracy", from = "ref") |>
  tr_link("pax", "erro:real")
```

## Como interpretar

Uma previsão (`series/forecast`), igual à de um modelo.

## Veja também

`series/accuracy` para comparar com o modelo; `series/forecast` para a previsão
de um modelo ajustado; `data/bind_rows` para juntar as tabelas de erro de
vários métodos numa só.
