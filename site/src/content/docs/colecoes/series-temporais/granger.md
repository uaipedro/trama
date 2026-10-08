---
title: "Causalidade de Granger"
description: "Uma série ajuda a prever as outras, além do passado delas? (precedência preditiva)"
section: colecoes
collection: series-temporais
node: series/granger
category: "Testes multivariados"
related: [series/var, series/var_select, series/irf]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Testa se a série (ou as séries) da CAUSA ajuda a prever as demais do VAR,
além do que as próprias demais já dizem sobre si. A hipótese nula é que a
causa NÃO Granger-causa o resto.

Isto é PRECEDÊNCIA PREDITIVA: se o passado de X melhora a previsão de Y, X vem
antes de Y e carrega informação útil. Não é causalidade estrutural. Uma
precedência pode vir de uma terceira variável, de expectativas ou de um
atraso de medição; rejeitar não prova que X produz Y.

Escolha o método pelo tipo de série:
- **Wald** (padrão): o F do próprio VAR. Pede séries ESTACIONÁRIAS. Com raiz
  unitária, o F não tem a distribuição nominal e o p-valor engana.
- **Toda-Yamamoto**: serve a séries em NÍVEL, integradas ou cointegradas. Reajusta
  o VAR com defasagens extras (a maior ordem de integração) e testa só as
  primeiras p. É o método seguro quando não se sabe se as séries são
  estacionárias.

O VECM não entra aqui: o bloco recebe o VAR em nível (series/var).

## Parâmetros

- **Causa** — uma ou mais séries, separadas por vírgula. A hipótese testada é
  que elas não Granger-causam as DEMAIS séries do VAR, em conjunto.
- **Método** — Wald (séries estacionárias) ou Toda-Yamamoto (nível, integradas
  ou cointegradas).

## Valor

Um teste (`data/test`) com H0 "causa não Granger-causa resto", a estatística
(F para Wald, qui-quadrado para Toda-Yamamoto) e o p-valor.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("g", "series/granger", causa = "dax", metodo = "toda_yamamoto", from = c(ajuste = "v"))
```

## Veja também

`series/var`, que ajusta o modelo; `series/var_select`, para escolher p antes;
`series/irf`, o efeito dinâmico de um choque.

### Como ler o card do teste

O topo diz o teste, as estrelas e a hipótese nula (**H0**). No meio, o número
grande e o **selo** da decisão ao nível de 5%: preenchido quando rejeita H0,
vazado quando não rejeita. Embaixo, a conclusão em uma linha.

**Quando o teste tem p-valor**, o número grande é o p-valor (abaixo de 1 em mil,
escrito em potência de dez) e embaixo dele vem a **régua**:

- a régua é o p-valor em escala logarítmica: quanto MAIS COMPRIDA a barra,
  MENOR o p-valor e mais forte a evidência contra H0. A ponta marca onde o
  p-valor está;
- as marcas são os cortes de 10%, 5%, 1% e 1 em mil; a barra enche de vez
  abaixo de 1 em dez mil;
- as **estrelas** são as do `summary()` do R: `***` abaixo de 1 em mil, `**`
  abaixo de 1%, `*` abaixo de 5%, `.` abaixo de 10% e `ns` acima. A cor da barra
  fica mais forte a cada estrela; sem estrela, cinza.

**Quando o teste só tem tabela de valores críticos** (sem p-valor), o número
grande é a estatística, e no lugar da régua vêm **três pontinhos**, dos cortes
de 10%, 5% e 1%:

- **preenchido** — a estatística passa do valor crítico daquele nível, na cauda
  que o teste usa;
- **na cor de destaque** — a decisão a 5% é rejeitar H0; **cinza** — não é: um
  ponto cinza preenchido é um teste que vence só o corte de 10%;
- **vazio** — não passa daquele corte.

Quantos pontos acendem diz a FOLGA da decisão, não uma decisão diferente.

A vista **detalhe** traz o registro inteiro: estatística, graus de liberdade,
p-valor ou valores críticos, o tamanho do efeito com o intervalo de 95%
desenhado contra a referência (quando o teste tem um), as partes de um teste
conjunto, a nota e a fonte.

Não rejeitar H0 não é provar H0: com poucas observações o teste deixa de
rejeitar por falta de poder. A conclusão diz "não há evidência", e não "é
igual", por isso.

