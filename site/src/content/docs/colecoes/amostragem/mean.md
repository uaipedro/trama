---
title: Média
description: Estima a média da população, com o erro padrão e o intervalo do desenho.
section: colecoes
collection: amostragem
node: sampling/mean
related: [sampling/total, sampling/ratio, sampling/plot_estimates, sampling/simulate]
---

## O que o bloco faz

A média da população, estimada pela média PONDERADA da amostra (Σ w·y / Σ w).
Numa AAS todo peso é igual e ela é a média simples; numa estratificada
desproporcional ou numa PPS, a média simples estaria errada.

## Quando usar

Use para estimar a média populacional de uma variável numérica, incluindo o desenho e seus pesos.

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
  tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop") |>
  tr_add("media", "sampling/mean", variavel = "producao_t", por = "regiao", from = "amostra")
```

## Como interpretar

A tabela de estimativas informa média ponderada, erro padrão, limites do intervalo, margem, coeficiente de variação, efeito do desenho, n e graus de liberdade. A estimação por domínio mantém o desenho e atribui contribuição zero às unidades fora do domínio. A saída é Estimativa `sampling/estimate` da média.

## Veja também

`sampling/total`, `sampling/ratio`, `sampling/plot_estimates`, `sampling/simulate`.
