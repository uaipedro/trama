---
title: Estratificada
description: Divide o cadastro em estratos e sorteia uma AAS dentro de cada um.
section: colecoes
collection: amostragem
node: sampling/stratified
related: [sampling/size_stratified, sampling/poststratify, sampling/simulate]
---

## O que o bloco faz

A amostra estratificada: o cadastro é dividido em estratos (regiões, redes,
faixas de tamanho) e uma AAS independente é sorteada em cada um. Ganha da AAS
quando os estratos diferem entre si — a variação ENTRE estratos sai do erro —,
e garante que todo estrato apareça na amostra.

O n de cada estrato vem do **plano** ligado (de `sampling/size_stratified`,
casando os estratos pelo nome) ou da **alocação** do n total:

- **proporcional** — n_h ∝ N_h;
- **igual** — o mesmo n em cada estrato;
- **neyman** — n_h ∝ N_h · S_h, com S_h o desvio da **variável do Neyman** no
  cadastro (a produção do censo anterior, a área).

Nenhum estrato recebe menos que 2 nem mais que a sua população. O peso de cada
unidade é N_h/n_h.

## Quando usar

Use quando os estratos são conhecidos no cadastro e devem estar representados, com alocação definida pelo plano ou pelo analista.

## Configuração

- **Estrato** — coluna do estrato no cadastro.
- **n total** — o n a alocar (sem plano).
- **Alocação** — `proporcional`, `igual` ou `neyman` (sem plano).
- **Variável do Neyman** — coluna numérica do cadastro; só no Neyman.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop")
```

## Como interpretar

A amostra conserva estrato e peso N_h/n_h. Proporcional mantém pesos iguais; Neyman aloca mais observações a estratos maiores e mais variáveis; a variância usa a variação dentro dos estratos. A saída é Amostra `sampling/sample` com estratos e pesos.

## Veja também

`sampling/size_stratified`, `sampling/poststratify`, `sampling/simulate`.
