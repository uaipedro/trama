---
title: Dois estágios
description: Sorteia conglomerados e, dentro de cada um, um número fixo de unidades.
section: colecoes
collection: amostragem
node: sampling/two_stage
related: [sampling/cluster, sampling/size_cluster, sampling/design]
---

## O que o bloco faz

A amostra em DOIS estágios: sorteia m conglomerados (primeiro estágio) e, dentro
de cada um, uma AAS de unidades (segundo estágio). É o desenho das pesquisas
domiciliares — setores, depois domicílios — e das avaliações escolares —
escolas, depois alunos.

Com o primeiro estágio **proporcional ao tamanho** e o mesmo número de unidades
em todo conglomerado, a amostra é AUTOPONDERADA: toda unidade da população tem
a mesma chance, e o peso é o mesmo para todas. É o padrão, e o que se usa quando
os conglomerados têm tamanhos muito diferentes.

Com **iguais**, o peso de cada unidade é (M/m) · (N_c/n_c).

Conglomerado menor que o número pedido entra inteiro. A variância é a do
conglomerado último — a variação entre os totais estimados dos conglomerados, que
já inclui a do segundo estágio.

## Quando usar

Use quando o cadastro permite selecionar conglomerados primeiro e uma amostra de unidades dentro de cada conglomerado depois.

## Configuração

- **Conglomerado** — coluna que identifica o conglomerado.
- **Conglomerados a sortear** — m.
- **Unidades por conglomerado** — n_c.
- **Primeiro estágio** — `iguais` ou `proporcional ao tamanho`.

Com o **plano** de `sampling/size_cluster` ligado, m e n_c vêm dele.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "escolas") |>
  tr_add("amostra", "sampling/two_stage", conglomerado = "escola", conglomerados = 30L,
         por_conglomerado = 10L, from = "pop")
```

## Como interpretar

A amostra contém as unidades sorteadas dentro dos conglomerados selecionados e pesos que combinam as probabilidades dos dois estágios. Conglomerados menores que o n solicitado entram integralmente. A saída é Amostra `sampling/sample` selecionada em dois estágios.

## Veja também

`sampling/cluster`, `sampling/size_cluster`, `sampling/design`.
