---
title: Tamanho para média
description: Quantas unidades sortear para estimar uma média com a margem de erro desejada?
section: colecoes
collection: amostragem
node: sampling/size_mean
related: [sampling/size_proportion, sampling/size_cluster, sampling/size_curve, sampling/srs]
---

## O que o bloco faz

O tamanho de uma amostra aleatória simples para estimar uma MÉDIA com margem
de erro E (metade do intervalo de confiança):

    n₀ = (z · S / E)²

em que S é o desvio padrão da variável e z o da confiança (1,96 a 95%). Depois
vêm os ajustes — deff, população finita e não resposta —, nessa ordem
(Cochran 1977).

O desvio vem de um campo ou de um **piloto**: ligue uma tabela na porta
`piloto` e escolha a variável, e o desvio e a média saem dela. Sem piloto, o
desvio pode vir de uma pesquisa anterior, ou da regra de bolso amplitude ÷ 4.

**Erro relativo** é a margem em % da média ("quero errar no máximo 10%"), que
é como a agronomia e a economia costumam pedir; precisa da média (do campo ou
do piloto).

## Quando usar

Use quando a variável de interesse é numérica e a decisão é o número de unidades necessário para atingir uma margem para a média.

## Configuração

- **Variável do piloto** — a coluna do piloto; só com a porta `piloto` ligada.
- **Desvio padrão** — S, sem piloto.
- **Média esperada** — só para erro relativo, sem piloto.
- **Margem de erro** — na unidade da variável (absoluto) ou em % da média
  (relativo).
- **Tipo de erro** — `absoluto` ou `relativo`.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("piloto", "sampling/srs", n = 30L, from = "pop") |>
  tr_add("plano", "sampling/size_mean", variavel = "producao_t", erro = 10,
         tipo_erro = "relativo", populacao = 2400) |>
  tr_link("piloto", "plano:piloto")
```

## Como interpretar

O plano registra o n arredondado e a escada de ajustes. Em erro relativo, a margem é uma fração da média esperada; com piloto, o desvio e a média são calculados dos valores observados. A saída é Plano `sampling/plan` com tamanho amostral e etapas de cálculo.

## Veja também

`sampling/size_proportion`, `sampling/size_cluster`, `sampling/size_curve`, `sampling/srs`.
