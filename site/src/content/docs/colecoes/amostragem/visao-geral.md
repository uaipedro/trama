---
title: Planejar e analisar uma amostra
description: Percorra o fluxo de amostragem desde o cadastro e o plano até a estimação e avaliação do desenho.
section: colecoes
collection: amostragem
order: 1
related: [sampling/example, sampling/size_mean, sampling/srs, sampling/design, sampling/mean, sampling/simulate]
---

## Fluxo de trabalho

A coleção `sampling` organiza as decisões na ordem de uma pesquisa: carregar ou declarar a população, planejar o tamanho, selecionar unidades ou registrar um desenho já coletado, calibrar pesos, estimar parâmetros e avaliar precisão. A saída dos blocos de seleção é uma amostra que conserva pesos, estratos e conglomerados para as estimativas seguintes.

| Etapa | Blocos | Resultado usado adiante |
| --- | --- | --- |
| Fonte | `sampling/example`, `sampling/design` | tabela de população ou amostra declarada |
| Planejamento | `sampling/size_mean`, `sampling/size_proportion`, `sampling/size_stratified`, `sampling/size_cluster`, `sampling/size_domains`, `sampling/size_curve` | plano e alocação |
| Seleção | `sampling/srs`, `sampling/systematic`, `sampling/stratified`, `sampling/pps`, `sampling/cluster`, `sampling/two_stage` | amostra com desenho |
| Ajuste | `sampling/poststratify`, `sampling/rake` | amostra com pesos calibrados |
| Estimação e avaliação | `sampling/mean`, `sampling/total`, `sampling/proportion`, `sampling/ratio`, `sampling/simulate` | estimativas, intervalos e diagnóstico |

## Exemplo completo

O cadastro `fazendas` é uma população simulada da coleção. A amostra estratificada preserva a região como estrato; a média seguinte usa os pesos do desenho.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop") |>
  tr_add("media", "sampling/mean", variavel = "producao_t", por = "regiao", from = "amostra")
```

A tabela resultante tem uma estimativa por região, além de erro padrão, intervalo e medidas de precisão. O desenho define a variância; filtros aplicados antes da estimação mudam essa população de referência. O [catálogo de Amostragem](/trama/colecoes/amostragem/) reúne as configurações e condições de cada bloco.
