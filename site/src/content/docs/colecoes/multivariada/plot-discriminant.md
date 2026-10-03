---
title: Plano discriminante
description: Plota escores das funções por grupo, com centróides e elipses.
section: colecoes
collection: multivariada
node: multi/plot_discriminant
related: [multi/discriminant_functions, models/confusion, models/predict]
---

## O que o bloco faz

O bloco `multi/plot_discriminant` plota os escores das funções discriminantes por grupo, com centróides e elipses opcionais. O bloco recebe o modelo (`models/fit`) de uma `multi/discriminant`.

## Quando usar

Use **Plano discriminante** para inspecionar visualmente separação e sobreposição entre grupos no espaço LD.

## Configuração

- **Função no eixo X** e
- **Função no eixo Y** — índices LD1/LD2 por padrão; devem ser diferentes.
- **Elipses de 95%** — ligadas por padrão. Opções visuais: **Aspecto**, **Tema**, **Título**, **Rótulo X**, **Rótulo Y** e **Legenda**.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.multi", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "multi/example", dataset = "iris") |>
  tr_add("lda", "multi/discriminant", resposta = "Species", from = "dados") |>
  tr_add("graf", "multi/plot_discriminant", from = "lda")
```

O plano LD1–LD2 mostra observações, centróides e dispersão por espécie.

## Como interpretar

Pontos são observações projetadas e centróides são médias dos grupos. Elipses resumem dispersão e não garantem regiões de classificação. O gráfico requer modelo linear; QDA não produz funções LD.

## Veja também

- [`Funções discriminantes`](/trama/colecoes/multivariada/discriminant-functions/)
- [`Matriz de confusão`](/trama/colecoes/modelos/confusion/)
- [`Prever`](/trama/colecoes/modelos/predict/)
