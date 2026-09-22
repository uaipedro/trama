---
title: Tamanho por conglomerados
description: Quantos conglomerados sortear, dado quanto as unidades de um mesmo conglomerado se parecem (ICC)?
section: colecoes
collection: amostragem
node: sampling/size_cluster
related: [sampling/cluster, sampling/two_stage, sampling/mean]
---

## O que o bloco faz

Leva um plano de AAS (de `sampling/size_mean` ou `sampling/size_proportion`) a
um plano de CONGLOMERADOS: escolas em vez de alunos, municípios em vez de
fazendas, setores em vez de domicílios.

Unidades do mesmo conglomerado se parecem, e cada uma a mais no mesmo
conglomerado traz menos informação nova. O quanto se parecem é a correlação
intraclasse (ICC, ρ), e o custo em variância é o efeito do desenho:

    deff = 1 + (m̄ − 1) · ρ

Com 20 alunos por escola e ρ = 0,05, deff = 1,95: a amostra de conglomerados
precisa de quase o dobro de alunos da AAS para a mesma margem. Com ρ = 0,25
(as `escolas`), deff = 5,75. É por isso que vale mais sortear MAIS escolas com
MENOS alunos em cada.

O n₀ do plano ligado é multiplicado pelo deff, dividido por m̄ (vira número de
conglomerados), corrigido pela população de conglomerados (M) e pela não
resposta do plano. O deff e a correção finita do plano ligado são trocados
pelos daqui, e a nota diz.

ICC de referência: 0,01 a 0,05 em características de domicílio por setor;
0,1 a 0,3 em desempenho escolar por escola.

## Quando usar

Use quando a coleta será agrupada e há um plano de referência AAS, um tamanho médio de conglomerado e uma ICC estimada.

## Configuração

- **Unidades por conglomerado (m̄)** — o tamanho médio que se vai entrevistar
  em cada conglomerado.
- **ICC** — a correlação intraclasse esperada.
- **Conglomerados na população** — M; 0 para infinitos.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("aas", "sampling/size_mean", desvio_padrao = 12, erro = 1.5) |>
  tr_add("plano_conglomerados", "sampling/size_cluster", tamanho_conglomerado = 15,
         icc = 0.25, conglomerados = 200, from = "aas")
```

## Como interpretar

O plano resultante substitui o deff original pela aproximação `1 + (m̄ − 1)·ICC`, converte unidades em conglomerados e aplica a correção finita quando M é conhecido. A saída é Plano `sampling/plan` ajustado para conglomerados.

## Veja também

`sampling/cluster`, `sampling/two_stage`, `sampling/mean`.
