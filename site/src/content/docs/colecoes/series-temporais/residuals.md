---
title: Resíduos
description: "Os resíduos do modelo, como série, para diagnóstico."
section: colecoes
collection: series-temporais
node: series/residuals
category: Modelar
order: 2
related: [series/ljung_box, series/acf]
---

## O que o bloco faz

O que o modelo NÃO explicou: a série menos o valor ajustado de um passo à
frente. Sai como série para que o diagnóstico seja feito com os mesmos nós de
qualquer série:

- `series/ljung_box` — sobrou autocorrelação? Se sim, o modelo deixou
  estrutura para trás, e os intervalos de previsão estão estreitos demais;
- `series/acf` — em que defasagem ela sobrou (uma barra na 12 é sazonalidade
  mal modelada);
- `view/histogram` (a série vira tabela sozinha no fio, com a coluna `valor`) —
  os resíduos parecem normais? Os intervalos supõem que sim.

Um modelo bom deixa resíduos sem padrão: média zero, variância constante, sem
autocorrelação.

## Quando usar

Extraia os erros do modelo como série para verificar estrutura restante. Resíduos autocorrelacionados indicam que o ajuste não capturou toda a dependência temporal.

## Configuração

Nenhum.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("arima", "series/arima", from = "pax") |>
  tr_add("res", "series/residuals", from = "arima") |>
  tr_add("lb", "series/ljung_box", graus = 2L, from = "res") |>
  tr_add("acf", "series/acf", from = "res")
```

## Como interpretar

Uma série (`series/ts`) dos resíduos. No Holt-Winters ela começa depois do
primeiro ciclo, que o método usa para iniciar.

## Veja também

`series/ljung_box` e `series/acf` para o diagnóstico; `view/histogram` para
a forma da distribuição.
