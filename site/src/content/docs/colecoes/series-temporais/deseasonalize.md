---
title: Tirar sazonalidade
description: "Estima o efeito de cada período do ciclo por regressão e o subtrai da série, mostrando os efeitos com erro-padrão e p-valor."
section: colecoes
collection: series-temporais
node: series/deseasonalize
category: Operar
order: 2
related: [series/regression, series/component, series/seasonality_kw]
---

## O que o bloco faz

`series/deseasonalize` estima o efeito de cada período do ciclo (cada mês, numa série mensal) por regressão em variáveis indicadoras e subtrai esse efeito da série, deixando a tendência e o resto. O card mostra os efeitos estimados, com erro-padrão e p-valor, como na `series/regression`. As saídas são `out`, a série dessazonalizada, e `sazonal`, o componente removido; somar as duas devolve a série original.

Com **Controlar a tendência** ligado (o padrão), o ajuste é `valor ~ t + periodo`, e só o efeito sazonal é removido. Isso importa numa série que cresce: dezembro tem média acima de janeiro só por vir depois, e as indicadoras sozinhas atribuiriam essa diferença à sazonalidade. Desligado, o ajuste é `valor ~ periodo`, e cada efeito é a média do período menos a média geral, o que só é correto para série sem tendência.

## Quando usar

Para retirar uma sazonalidade fixa e aditiva antes de olhar a tendência ou de aplicar um teste que pede série sem ciclo. Se a amplitude da oscilação cresce com o nível da série, aplique `series/transform` com `log` antes. Se a sazonalidade muda ao longo dos anos, a decomposição indicada é a `series/stl`.

## Configuração

- **Controlar a tendência** — inclui `t` no ajuste (padrão).
- **Contraste** — `soma_zero` (desvio da média do ciclo) ou `categoria_base` (diferença para o primeiro período).
- **Excluir termos sazonais** — nomes ou números separados por vírgula; os períodos excluídos formam o grupo **demais**.
- **Remover termos sazonais não significativos** e **Confiança da remoção** — remoção para trás; os p-valores finais ficam otimistas.

O bloco não aceita valores faltantes (interpole antes com `series/interpolate`) e pede série com ciclo, isto é, frequência maior que 1. Com ciclos incompletos, ele avisa.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", metodo = "log", from = "pax") |>
  tr_add("dessaz", "series/deseasonalize", from = "log") |>
  tr_add("efeito", "series/plot", from = "dessaz:sazonal")
```

## Como interpretar

Cada efeito estimado é o quanto aquele período fica, em média, acima ou abaixo do nível da série, descontada a tendência. Os p-valores dos efeitos individuais não substituem um teste da sazonalidade como um todo: para o F da sazonalidade, ajuste a `series/regression` com a mesma fórmula e leia o `models/anova_table`; sem supor normalidade, use a `series/seasonality_kw`. A sazonalidade removida é a mesma em todos os anos.
