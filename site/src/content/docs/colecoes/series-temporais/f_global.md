---
title: F global
description: "Teste F do modelo inteiro: a regressão explica alguma coisa?"
section: colecoes
collection: series-temporais
node: series/f_global
category: Sazonalidade
order: 2
related: [series/regression, series/f_sazonal, series/f_tendencia]
---

## O que o bloco faz

Testa o ajuste de `series/regression` INTEIRO. H0 é "todos os coeficientes,
fora o intercepto, são nulos" — nenhum termo explica a série: p-valor pequeno quer dizer que o modelo — tendência e sazonalidade
juntas — captura parte do movimento.

Diz que há sinal, não de onde ele vem. Para separar, `series/f_sazonal` e
`series/f_tendencia`, que testam cada bloco por si.

### Por que em BLOCO

É a razão de os três blocos de F existirem. Com onze dummies mensais, olhar
onze p-valores é onze chances de encontrar um "significativo" por acaso; a
pergunta honesta é se o CONJUNTO delas melhora o ajuste. Os coeficientes um a
um continuam disponíveis: ligue a regressão num nó da `data`.

### Com um bloco só, este teste se repete

Numa regressão sem sazonalidade (ou de grau 0), o modelo tem um bloco de
termos apenas — e aí o F global e o F parcial daquele bloco são o MESMO teste.
Os dois cards mostram números idênticos. É esperado, não é defeito.

### Antes de citar o p-valor

O teste supõe erro sem autocorrelação. Série temporal quase nunca obedece, e o
efeito é conhecido: o p-valor sai otimista demais. Extraia o resto com
`series/component` e passe pelo `series/ljung_box`; se houver autocorrelação,
os p-valores daqui são indicativos, não conclusivos.

## Quando usar

Avalie em conjunto se os termos da regressão dos componentes explicam a série. Use o resultado junto aos coeficientes, ao ajuste e à inspeção dos resíduos.

## Configuração

Nenhum. Uma entrada: **ajuste**, vindo de `series/regression`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("reg", "series/regression", from = "pax") |>
  tr_add("f", "series/f_global", from = "reg")
```

## Como interpretar

Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta os três F num só quadro.

## Veja também

`series/f_sazonal` e `series/f_tendencia`, o mesmo F bloco a bloco;
`series/regression`, que produz o ajuste; `series/ljung_box` para conferir a
autocorrelação do resto.

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
