---
title: "Portmanteau multivariado"
description: "Box-Pierce e Ljung-Box multivariados: os resíduos do VAR ou VECM são autocorrelacionados?"
section: colecoes
collection: series-temporais
node: series/portmanteau_mv
category: "Testes multivariados"
related: [series/normality_mv, series/arch_mv, series/residuals_mv]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Testa se os resíduos do modelo ainda têm autocorrelação, somando as
autocorrelações cruzadas até a defasagem `h`. H0 é a AUSÊNCIA de
autocorrelação até `h`: rejeitar diz que o VAR (ou VECM) deixou dinâmica de
fora, e o caminho é aumentar `p` ou revisar o modelo.

As duas versões são as de Hosking (1980). O Box-Pierce (`box_pierce`) usa a
aproximação assintótica. O Ljung-Box (`ljung_box`, o padrão) corrige para
amostra finita, multiplicando cada termo por `T/(T - j)`; é o que o `vars`
chama de `PT.adjusted`, e o que se deve ler quando `T` não é grande.

Graus de liberdade: `K² (h − p)`, com `K` o número de séries e `p` a ordem do
VAR. Isso exige `h` bem maior que `p`: com `h` perto de `p` os graus de
liberdade somem, e a aproximação qui-quadrado fica frouxa. Para VECM, o `vars`
acrescenta `K` aos graus de liberdade; o bloco segue o pacote, e o número vai
no card.

Defasagens automáticas (`defasagens = 0`): `h = min(16, ⌊T/5⌋)`, com `T` o número
de observações dos resíduos. O 16 é o padrão do `vars` (Pfaff, 2008). O teto
`T/5` mantém o termo `T/(T − j)` perto de 1 até a última defasagem, e a
aproximação qui-quadrado sem depender de amostra grande. Se `h` der `p` ou
menos, o bloco recusa: não há graus de liberdade.

## Parâmetros

- **Estatística** — `ljung_box` (padrão, versão ajustada) ou `box_pierce`
  (assintótica).
- **Defasagens (h)** — número de defasagens somadas. 0 aplica a regra acima.

## Valor

Um teste (`data/test`): estatística qui-quadrado, p-valor, e nota com `h` e os
graus de liberdade.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("a", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("b", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", from = c("a", "b")) |>
  tr_add("v", "series/var", defasagens = 2L, from = "j") |>
  tr_add("lb", "series/portmanteau_mv", defasagens = 12L, from = c(modelo = "v"))
```

## Veja também

`series/normality_mv` e `series/arch_mv` para os outros dois diagnósticos dos
resíduos; `series/residuals_mv` para ver os resíduos.

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

