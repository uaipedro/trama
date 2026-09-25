---
title: F do bloco sazonal
description: "Teste F do bloco sazonal da regressão: há sazonalidade?"
section: colecoes
collection: series-temporais
node: series/f_seasonal
category: Sazonalidade
order: 2
related: [series/regression, series/f_trend, series/f_global]
---

## O que o bloco faz

Testa os coeficientes sazonais de um ajuste de `series/regression` EM BLOCO.
H0 é "os coeficientes sazonais são todos nulos": rejeitar é concluir que há
sazonalidade.

O F parcial compara o ajuste com e sem o bloco — reajusta a regressão sem as
dummies e mede o quanto o encaixe piorou.

### Por que em BLOCO

Com onze dummies mensais, olhar onze p-valores é onze chances de encontrar um
"significativo" por acaso. A pergunta honesta é se o CONJUNTO delas melhora o
ajuste, e é o que este teste mede. Os coeficientes um a um continuam
disponíveis: ligue a regressão num nó da `data`.

### A regressão precisa ter o bloco

Regressão ligada sem sazonalidade é cartão vermelho, e não um card verde
dizendo que não há: o bloco não foi testado, ele nunca existiu. Ligue a
sazonalidade no `series/regression`.

## Quando usar

Teste se o bloco de termos sazonais da regressão contribui para explicar a série, mantendo os outros termos do modelo.

## Configuração

Nenhum. Uma entrada: **ajuste**, vindo de `series/regression` com sazonalidade
ligada.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("reg", "series/regression", from = "pax") |>
  tr_add("f", "series/f_seasonal", from = "reg")
```

## Como interpretar

Um teste (`series/test`). Ligado numa entrada de tabela, vira uma linha de
relatório.

## Veja também

`series/f_trend`, o mesmo teste no outro bloco; `series/f_global`, o
modelo inteiro; `series/seasonal_plot` para ver a sazonalidade que o teste
mede.

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
