---
title: PPS
description: Sorteia com probabilidade proporcional a uma medida de tamanho (PPS sistemática).
section: colecoes
collection: amostragem
node: sampling/pps
related: [sampling/total, sampling/cluster, sampling/two_stage]
---

## O que o bloco faz

A amostra com probabilidade proporcional ao tamanho (PPS): a unidade com o
dobro da área tem o dobro da chance. Quando a variável de interesse é
proporcional ao tamanho (a produção de uma fazenda cresce com a área), o
estimador do total de Horvitz-Thompson quase não varia: cada unidade sorteada
"representa" um pedaço da população do tamanho dela. Nas `fazendas`, o deff do
total fica perto de 0,1.

O sorteio é o sistemático de Madow na ordem do cadastro: as probabilidades
π_i = n · x_i / X são acumuladas, e um começo aleatório com passo 1 escolhe as
unidades. Unidade com π_i ≥ 1 entra com certeza, e a conta é refeita com as
outras. Ordenar o cadastro antes (um `data/arrange`) soma estratificação
implícita.

O peso é 1/π_i. A variância é a com reposição (Hansen-Hurwitz), levemente
conservadora; as unidades de certeza não contribuem.

## Quando usar

Use quando existe medida de tamanho positiva no cadastro completo e a variável de interesse tende a crescer com ela.

## Configuração

- **Medida de tamanho** — coluna numérica positiva, conhecida para TODO o
  cadastro (área, número de empregados, população do município).
- **n** — quantas unidades sortear (ou o **plano** ligado).

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/pps", tamanho = "area_ha", n = 100L, from = "pop")
```

## Como interpretar

Cada unidade tem probabilidade de inclusão proporcional à medida de tamanho e peso inverso dessa probabilidade. Unidades com probabilidade de inclusão pelo menos 1 entram com certeza. A saída é Amostra `sampling/sample` com probabilidades de inclusão PPS e pesos.

## Veja também

`sampling/total`, `sampling/cluster`, `sampling/two_stage`.
