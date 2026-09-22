---
title: Tamanho estratificado
description: Quantas unidades sortear, e quantas em cada estrato, para a margem de erro desejada?
section: colecoes
collection: amostragem
node: sampling/size_stratified
related: [sampling/stratified, sampling/size_mean, sampling/size_curve]
---

## O que o bloco faz

Calcula o n de uma amostra ESTRATIFICADA e o reparte entre os estratos. A
entrada é a tabela de estratos: uma linha por estrato, com o tamanho na
população (N_h) e o desvio da variável dentro dele (S_h) — é o que um
`data/group_summarise` do cadastro dá (`n()` e `sd(x)`), ou o que uma pesquisa
anterior publicou.

As alocações:

- **proporcional** — n_h ∝ N_h. A amostra é um retrato da população, e a
  autoponderada: todo mundo tem o mesmo peso.
- **neyman** — n_h ∝ N_h · S_h. Mais amostra onde há mais população E mais
  variação. É a de menor variância para um n fixo, e o ganho é grande quando os
  estratos diferem em variabilidade (o Norte das `fazendas`).
- **ótima** — n_h ∝ N_h · S_h / √c_h. Neyman com custo: estrato caro recebe
  menos. É a de menor variância para um ORÇAMENTO fixo.
- **igual** — o mesmo n em todo estrato. Serve para comparar estratos entre si,
  e não para a média geral.

Com **Margem de erro**, o n sai da variância da média estratificada igualada a
(E/z)²; com **n total**, o n é dado e o card mostra a margem que ele alcança.
Nenhum estrato recebe menos que 2 (sem isso a variância dele não existe) nem
mais que N_h (o excedente vira censo e é redistribuído).

Para uma proporção, ponha em S_h a raiz de p_h(1 − p_h).

## Quando usar

Use quando os tamanhos e desvios por estrato são conhecidos e o plano deve repartir a amostra entre eles.

## Configuração

- **Estrato**, **Tamanho (N_h)**, **Desvio (S_h)** — colunas da tabela de
  estratos.
- **Custo** — coluna do custo por unidade; só na alocação ótima.
- **Alocação** — `proporcional`, `neyman`, `ótima` ou `igual`.
- **Margem de erro da média** — E, na unidade da variável.
- **n total** — n fixo; quando maior que 0, vence a margem.
- **Confiança**, **Taxa de resposta** — como em `sampling/size_mean`; a taxa
  infla cada estrato (até N_h).

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("estratos", "sampling/example", dataset = "estratos_fazendas") |>
  tr_add("plano", "sampling/size_stratified", estrato = "regiao", tamanho = "N",
         desvio = "desvio_producao", alocacao = "neyman", erro = 60, from = "estratos")
```

## Como interpretar

O plano devolve uma alocação por estrato. Compare `n_final` com `N_h`: estratos censitários têm `n_final = N_h`; a taxa de resposta aumenta os contatos previstos, sem aumentar o número de respostas. A saída é Plano `sampling/plan` com uma alocação por estrato.

## Veja também

`sampling/stratified`, `sampling/size_mean`, `sampling/size_curve`.
