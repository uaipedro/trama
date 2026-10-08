---
title: "Normalidade multivariada"
description: "Jarque-Bera multivariado: os resíduos são normais multivariados?"
section: colecoes
collection: series-temporais
node: series/normality_mv
category: "Testes multivariados"
related: [series/portmanteau_mv, series/arch_mv]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Testa a normalidade conjunta dos resíduos do VAR ou VECM, pela versão
multivariada do Jarque-Bera. H0 é a normalidade multivariada. A estatística
soma uma parte de ASSIMETRIA e uma de CURTOSE, cada uma com `K` graus de
liberdade; o teste conjunto tem `2K`.

As duas partes saem separadas no campo `extra` (`assimetria` e `curtose`),
porque a rejeição costuma vir de uma só: resíduos com caudas pesadas rejeitam
pela curtose, e a assimetria pode estar em ordem.

Rejeitar não invalida a estimação por mínimos quadrados, mas invalida os
intervalos e testes baseados na normalidade (em amostra pequena, sobretudo).

## Parâmetros

Este bloco não tem parâmetros.

## Valor

Um teste (`data/test`). Assimetria e curtose separadas no `extra`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", from = c("a", "b")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("jb", "series/normality_mv", from = c(modelo = "v"))
```

## Veja também

`series/portmanteau_mv` e `series/arch_mv` para os outros diagnósticos dos
resíduos.

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

