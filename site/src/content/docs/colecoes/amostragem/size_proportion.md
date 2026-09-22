---
title: Tamanho para proporção
description: Quantas unidades sortear para estimar uma proporção com a margem de erro desejada?
section: colecoes
collection: amostragem
node: sampling/size_proportion
related: [sampling/size_mean, sampling/size_domains, sampling/size_cluster, sampling/proportion]
---

## O que o bloco faz

O tamanho de uma amostra aleatória simples para estimar uma PROPORÇÃO p com
margem de erro E:

    n₀ = z² · p(1 − p) / E²

Sem ideia de p, use **0,5**: é o pior caso, e o n que sai serve para qualquer
proporção — é por isso que pesquisa de opinião com margem de 3 pontos e 95% tem
sempre perto de 1.068 entrevistas. Com p = 0,1 o n cai para um terço.

A margem é em PONTOS de proporção: 0,05 são 5 pontos (40% ± 5%), e não 5% de
40%.

## Quando usar

Use para definir o n antes de coletar respostas categóricas; p = 0,5 cobre o pior caso quando a proporção ainda é desconhecida.

## Configuração

- **Proporção esperada** — p, entre 0 e 1.
- **Margem de erro** — em pontos de proporção (0,03 = 3 pontos).

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("plano", "sampling/size_proportion", erro = 0.03, taxa_resposta = 0.7)
```

## Como interpretar

A saída é um plano de amostragem. Com p = 0,5, a variância binomial é máxima e o n serve como limite conservador; a margem é dada em pontos de proporção. A saída é Plano `sampling/plan` com o tamanho para estimar uma proporção.

## Veja também

`sampling/size_mean`, `sampling/size_domains`, `sampling/size_cluster`, `sampling/proportion`.
