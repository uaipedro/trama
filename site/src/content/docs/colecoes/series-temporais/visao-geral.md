---
title: Séries temporais
description: "Monte, transforme, modele e interprete séries mantendo frequência, calendário e sequência de análise explícitos."
section: colecoes
collection: series-temporais
order: 1
related: [series/example, series/from_table, series/diff, series/decompose, series/arima, series/forecast, series/plot]
---

## O calendário faz parte dos dados

Uma série temporal reúne valores em uma ordem de tempo e carrega uma frequência. A frequência informa quantas observações formam um ciclo: 12 em dados mensais, 4 em trimestrais e 1 em anuais. Diferenças sazonais, decomposições e gráficos sazonais usam esse valor para localizar ciclos; por isso, a frequência precisa representar o calendário real.

## Caminho de trabalho

| Etapa | Blocos | Resultado |
| --- | --- | --- |
| Obter ou montar a série | `series/example`, `series/from_table` | Uma série `series/ts` com início e frequência |
| Preparar a sequência | `series/interpolate`, `series/transform`, `series/diff` | Série adequada à pergunta ou ao modelo |
| Separar componentes | `series/decompose`, `series/stl`, `series/component` | Tendência, sazonalidade e resto |
| Ajustar e prever | `series/arima`, `series/ets`, `series/forecast` | Modelo e valores futuros |
| Conferir e comunicar | `series/residuals`, `series/accuracy`, `series/plot_forecast` | Diagnóstico, medidas de erro e gráfico |

## Exemplo com uma série do R

`AirPassengers` contém passageiros aéreos mensais de 1949 a 1960. A amplitude sazonal cresce junto com o nível, então a transformação logarítmica torna essa variação mais estável antes da decomposição. O fluxo abaixo mantém a sequência visível:

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example", dataset = "AirPassengers") |>
  tr_add("log", "series/transform", metodo = "log", from = "pax") |>
  tr_add("decomp", "series/stl", from = "log") |>
  tr_add("tendencia", "series/component", componente = "tendencia", from = "decomp")
```

## Séries próprias

Leia e prepare os dados como tabela na coleção Dados. `series/from_table` ordena pela coluna de tempo e verifica datas repetidas ou períodos ausentes nos calendários que reconhece. Para meses sem observação, mantenha a linha com valor `NA`; `series/interpolate` permite escolher como tratar esses pontos. Sem coluna de tempo, a ordem das linhas é preservada e o início precisa ser informado quando não for o padrão.

## Perguntas de diagnóstico

Os testes estão agrupados pela hipótese que respondem: raiz unitária, autocorrelação, tendência, sazonalidade e regressão. Cada página descreve a hipótese nula, a estatística e o significado do resultado; p-valor não mede o tamanho ou a importância prática do efeito. Compare testes com o gráfico e com o conhecimento sobre o processo que gerou os dados.

## Frequência e interpretação

Uma frequência incorreta desloca a interpretação sazonal em todo o fluxo. A diferença sazonal usa a defasagem igual à frequência; gráficos ACF marcam múltiplos dela; métodos sazonais também dependem dessa convenção. Séries anuais têm frequência 1 e não oferecem ciclo sazonal. Em séries diárias ou horárias, a frequência deve corresponder ao ciclo que a análise pretende representar.
