---
title: Linha
description: Conecte valores ao longo de um eixo que tenha ordem definida.
section: colecoes
collection: visualizacao
node: view/line
category: relacao
related: [view/points, view/area, data/convert]
---

## O que o bloco faz

`view/line` liga os valores de Y seguindo a ordem de X. Quando **Uma linha por** está preenchido, cada grupo recebe sua própria série; quando está vazio, todas as linhas formam uma série.

## Quando usar

Use quando X representa tempo, dose, posição ou outra ordem em que a ligação entre observações tenha significado. Para categorias sem ordem, como nomes de produto, use `view/points`.

## Configuração

**Eixo X** e **Eixo Y** são obrigatórios. **Uma linha por** (**Cor por** na chamada R) identifica séries independentes; preencha quando várias séries compartilham os mesmos valores de X. **Marcar pontos** acrescenta uma marca por observação. **Eixo em log** aceita `nenhum`, `X`, `Y` ou `ambos` e requer valores positivos. **Painéis por** separa grupos em painéis de escala comum. Para X textual, a ordem é alfabética; use fator com níveis ordenados ou uma coluna de data quando a ordem for temporal.

## Exemplo

```r
library(trama.view)
dados <- data.frame(
  mes = as.Date(c("2026-01-01", "2026-02-01", "2026-01-01", "2026-02-01")),
  regiao = c("Norte", "Norte", "Sul", "Sul"), receita = c(12, 15, 8, 10)
)
tr_line(dados, x = "mes", y = "receita", cor = "regiao", marcar = TRUE)
```

## Como interpretar

Cada segmento mostra a variação entre dois X consecutivos dentro de uma série. Uma lacuna em Y interrompe a linha, indicando que não há observação naquele trecho. Confira a ordem de X e o campo Uma linha por: sem separar séries, pontos de grupos diferentes podem ser conectados numa trajetória que não corresponde aos dados.
