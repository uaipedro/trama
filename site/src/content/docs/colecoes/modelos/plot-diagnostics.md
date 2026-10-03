---
title: Diagnóstico dos resíduos
description: "Quatro painéis: resíduos × ajustados, Q-Q normal, escala-locação e histograma."
section: colecoes
collection: modelos
node: models/plot_diagnostics
category: resumir
related: [models/anova_table, models/coefficients]
---

## O que o bloco faz

`models/plot_diagnostics` Desenha resíduos versus ajustados, Q-Q normal, escala-localização e histograma. A saída é `view/plot`.

## Quando usar

Use após ajustar regressão ou ANOVA para inspecionar forma, normalidade e dispersão residual em conjunto.

## Configuração

Não há parâmetros obrigatórios adicionais; aspecto e tema controlam apresentação.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados") |>
  tr_add("resultado", "models/plot_diagnostics", from = "ajuste")
```

Os painéis usam o ajuste `weight ~ group`: resíduos versus ajustados examina padrão e dispersão, Q-Q compara quantis, escala-localização mostra variância e histograma resume a forma.

## Como interpretar

Curvatura, desvios no Q-Q ou dispersão crescente indicam aspectos do ajuste a investigar; nenhum painel, sozinho, identifica uma correção.
