---
title: Gráfico de estimativas
description: Estimativas por domínio com o intervalo de confiança, coloridas pela precisão (CV).
section: colecoes
collection: amostragem
node: sampling/plot_estimates
related: [sampling/mean, sampling/proportion, sampling/total, sampling/ratio]
---

## O que o bloco faz

O gráfico de uma estimativa: um ponto e o intervalo de confiança por domínio (e
por categoria, na proporção), coloridos pela faixa de precisão do CV — ótima,
boa, regular, imprecisa. É a figura do relatório: o domínio cujo intervalo é
largo demais para publicar aparece em outra cor sem legenda a decifrar.

Proporções saem em %.

## Quando usar

Use para apresentar estimativas por domínio com intervalos e indicação visual de precisão.

## Configuração

Só os de aparência.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop") |>
  tr_add("proporcao", "sampling/proportion", variavel = "irrigada", nivel = "sim", por = "regiao",
         from = "amostra") |>
  tr_add("grafico", "sampling/plot_estimates", titulo = "Fazendas irrigadas por região", from = "proporcao")
```

## Como interpretar

Cada ponto representa uma estimativa por domínio ou categoria e a barra seu intervalo de confiança. O coeficiente de variação orienta a classe visual de precisão; proporções são apresentadas em porcentagem. A saída é Gráfico `view/plot` das estimativas e intervalos.

## Veja também

`sampling/mean`, `sampling/proportion`, `sampling/total`, `sampling/ratio`.
