---
title: Calibrar (raking)
description: Ajusta os pesos para que a amostra bata os totais conhecidos de várias variáveis ao mesmo tempo.
section: colecoes
collection: amostragem
node: sampling/rake
related: [sampling/poststratify, sampling/design, sampling/simulate]
---

## O que o bloco faz

A calibração por várias variáveis: os pesos são ajustados para que a amostra
tenha, ao mesmo tempo, o total conhecido de cada categoria de cada variável —
sexo, raça/cor, faixa etária, capital. É o ajuste proporcional iterativo
(raking): uma variável por vez, em rodadas, até todas baterem.

Diferente de `sampling/poststratify`, não precisa do total de cada CRUZAMENTO
(mulher preta de 18 a 24 anos em Belém), que o Censo raramente publica e a
amostra raramente preenche: bastam as margens de cada variável.

A tabela de **totais** é longa, uma linha por categoria: a coluna da variável
tem o NOME da coluna da amostra (`sexo`), a da categoria o valor (`mulher`), e a
do total a população. Todas as variáveis têm de somar a mesma população (tolerância
de 1%), e toda categoria precisa de gente na amostra: junte as vazias antes.

Numa coleta não probabilística (indicação, adesão aberta), calibrar é o mínimo:
não torna a amostra aleatória, mas faz o perfil dela ser o da população. A
variância usa os resíduos da regressão nas variáveis calibradas, e a nota da
amostra traz a amplitude dos pesos — pesos muito desiguais (razão acima de uns
10) inflam a variância e pedem categorias mais largas.

## Quando usar

Use para calibrar simultaneamente margens populacionais conhecidas de diversas variáveis categóricas.

## Configuração

- **Coluna da variável**, **Coluna da categoria**, **Coluna do total** — colunas
  da tabela de totais.
- **Rodadas** — máximo de rodadas do ajuste.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("escolas", "sampling/example", dataset = "escolas") |>
  tr_add("perfil", "sampling/example", dataset = "perfil_escolas") |>
  tr_add("aas", "sampling/srs", n = 300L, from = "escolas") |>
  tr_add("calibrada", "sampling/rake", from = "aas") |>
  tr_link("perfil", "calibrada:totais")
```

## Como interpretar

A saída é uma amostra com pesos calibrados. Confira se os totais de cada variável somam a mesma população e observe a amplitude dos pesos: pesos muito desiguais podem ampliar a variância. A saída é Amostra `sampling/sample` com pesos calibrados.

## Veja também

`sampling/poststratify`, `sampling/design`, `sampling/simulate`.
