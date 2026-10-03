---
title: Área
description: Mostra o total e a composição de séries ao longo de um eixo ordenado.
section: colecoes
collection: visualizacao
node: view/area
category: relacao
related: [view/line, view/bars, data/group_summarise]
---

## O que o bloco faz

`view/area` mostra o total e a composição de séries ao longo de um eixo ordenado.

## Quando usar

Use quando séries positivas compõem um total ao longo do tempo ou de outra ordem. O eixo X deve ser numérico ou data; Y deve ser numérico.

## Configuração

Empilhar por separa séries. Painéis por divide grupos em gráficos de mesma escala. A faixa inferior parte do zero; faixas superiores não permitem comparar magnitudes com a mesma precisão. X e Y são obrigatórios.

## Exemplo

```r
library(trama.view)
dados <- data.frame(
  mes = as.Date(c("2026-01-01", "2026-01-01", "2026-02-01", "2026-02-01")),
  regiao = c("Norte", "Sul", "Norte", "Sul"), receita = c(12, 8, 15, 10)
)
tr_area(dados, x = "mes", y = "receita", cor = "regiao")
```

## Como interpretar

Cada faixa colorida mostra a contribuição da série à altura total naquele X; o topo da pilha é a soma. A faixa inferior parte do zero e permite ler sua magnitude. As faixas superiores flutuam sobre as anteriores, então compare a composição com cuidado; para comparar trajetórias entre séries, use `view/line`.
