---
title: Holt-Winters
description: "Suavização exponencial clássica com nível, tendência e sazonalidade."
section: colecoes
collection: series-temporais
node: series/holt_winters
category: Modelar
order: 2
related: [series/ets, series/forecast]
---

## O que o bloco faz

O método de Holt-Winters como o `stats` o implementa: três equações de
suavização exponencial — nível, tendência e sazonalidade —, com as constantes
α, β e γ escolhidas por mínimos quadrados dos erros de um passo.

É o método que se ensina e se cita, e fica ao lado do `series/ets`, que o
generaliza. Desligar **Tendência** dá a suavização de Holt sem tendência;
desligar também **Sazonalidade** dá a suavização exponencial simples. O efeito
de cada chave aparece na previsão do card seguinte.

Com sazonalidade, pede série sazonal com dois ciclos, e não aceita faltantes.
Não tem AIC (não é ajustado por verossimilhança), então o resumo o mostra em
branco.

## Quando usar

Ajuste a forma clássica de suavização de Holt–Winters para uma série com nível, tendência e sazonalidade. A frequência precisa representar o ciclo sazonal.

## Configuração

- **Tendência** — estima a inclinação (β).
- **Sazonalidade** — estima o padrão sazonal (γ).
- **Sazonalidade do tipo** — `aditiva` ou `multiplicativa`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("co2", "series/example", dataset = "co2") |>
  tr_add("hw", "series/holt_winters", from = "co2") |>
  tr_add("prev", "series/forecast", horizonte = 36L, from = "hw")
```

## Como interpretar

Um modelo (`series/model`).

## Veja também

`series/ets`, a versão em espaço de estados; `series/forecast` para prever.
