---
title: Total
description: Estima o total da população (Horvitz-Thompson), com o erro do desenho.
section: colecoes
collection: amostragem
node: sampling/total
related: [sampling/mean, sampling/ratio, sampling/plot_estimates]
---

## O que o bloco faz

O total da população pelo estimador de Horvitz-Thompson: Σ w·y, em que cada
unidade da amostra conta pelas w unidades da população que representa. É o que
responde "quanto se produziu na região", "quantos domicílios têm internet" (o
total de um indicador 0/1).

O total depende do peso inteiro, e não só das proporções entre os pesos: uma
amostra sem peso (ou declarada sem ele) daria o total da AMOSTRA.

## Quando usar

Use para estimar uma quantidade total da população, como produção ou número de unidades com certa característica.

## Configuração

- **Variável** — coluna numérica.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/pps", tamanho = "area_ha", n = 100L, from = "pop") |>
  tr_add("total", "sampling/total", variavel = "producao_t", por = "regiao", from = "amostra")
```

## Como interpretar

O total de Horvitz–Thompson soma valores ponderados; depende da escala dos pesos. A tabela também apresenta erro padrão e intervalo calculados segundo o desenho. A saída é Estimativa `sampling/estimate` do total.

## Veja também

`sampling/mean`, `sampling/ratio`, `sampling/plot_estimates`.
