---
title: "Engle-Granger"
description: "Cointegração de Engle-Granger: a regressão dos resíduos tem raiz unitária?"
section: colecoes
collection: series-temporais
node: series/engle_granger
category: "Testes multivariados"
related: [series/johansen, series/vecm, series/adf, series/kpss]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Testa se a série RESPOSTA é cointegrada com as demais, isto é, se existe uma
combinação linear delas que é estacionária.

A regressão da resposta nas outras séries é estimada por mínimos quadrados. O
teste é o ADF SEM constante nos resíduos dessa regressão, com H0 = não há
cointegração. Rejeitar conclui cointegradas.

### Por que não o ADF comum

Os resíduos de uma regressão já estimada são MENORES do que um resíduo qualquer:
o ajuste os aproxima de zero. Os críticos do ADF comum são otimistas nesse caso
e rejeitam demais. O p-valor sai da superfície de MacKinnon para N variáveis
(N = resposta mais regressoras), que é a distribuição certa.

### Limites

Com mais de duas séries, o teste dá UMA relação, e o resultado depende de qual
série é a resposta. Para contar as relações de uma vez, use o `series/johansen`.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Parâmetros

- **Resposta** — a série explicada; as outras da série múltipla são as
  regressoras.
- **Determinístico** — constante (padrão), ou constante com tendência na regressão.
- **Defasagens do ADF** — teto das defasagens do ADF nos resíduos; 0 usa a regra
  automática, e o AIC escolhe dentro dele.

## Valor

Um teste (`data/test`). A estatística é o t do ADF nos resíduos; o p-valor é o
de MacKinnon para N variáveis, e a decisão a 5% sai dele.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("j", "series/join", nomes = "dax, cac", from = c("dax", "cac")) |>
  tr_add("eg", "series/engle_granger", resposta = "dax", from = "j")
```

## Veja também

`series/johansen`, que conta as relações de cointegração de uma vez; `series/vecm`,
para modelar as séries cointegradas; `series/adf` e `series/kpss`, para checar que
cada série é I(1) antes.

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

