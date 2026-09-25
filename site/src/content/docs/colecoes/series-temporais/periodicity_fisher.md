---
title: Periodicidade (Fisher)
description: "Fisher: existe uma periodicidade escondida?"
section: colecoes
collection: series-temporais
node: series/periodicity_fisher
category: Sazonalidade
order: 2
related: [series/seasonality_kw, series/interpolate, series/window]
---

## O que o bloco faz

Procura uma PERIODICIDADE ESCONDIDA. Decompõe a série nas ondas de todos os
períodos que ela comporta — o periodograma — e pergunta se o MAIOR pico é maior
do que o acaso produziria. A estatística é o **g**, a fração da potência total
que esse pico sozinho carrega (eq. 3.41 da dissertação). H0 é "a série não tem
periodicidade", e rejeitar é concluir que há um ciclo.

É o irmão do `series/seasonality_kw`, e os dois fazem a pergunta em sentidos
opostos. O Kruskal-Wallis COMPARA ESTAÇÕES QUE VOCÊ JÁ DECLAROU: ele lê a
frequência da série para saber o que é janeiro, e responde se aquelas estações
diferem. Este aqui CAÇA UM PERÍODO DESCONHECIDO: não pergunta a frequência a
ninguém, varre a grade inteira e diz onde está o pico. Quem já sabe qual é o
ciclo e quer saber se ele é real usa o Kruskal-Wallis; quem desconfia de um ciclo
e não sabe qual, usa este.

### O pico pode não ser uma estação

É o ponto que mais engana desta página: o teste acha o maior pico ONDE QUER QUE
ELE ESTEJA, e nem todo pico é estação. Cinco séries do R, medidas:

```
série            n      g       zα (5%)   p            período do pico
AirPassengers   72   0.5017    0.0974    2.392e-20     12 observações
UKgas           54   0.5531    0.1235    1.566e-17      4 observações
nottem         120   0.9130    0.0633    7.636e-125    12 observações
lh              24   0.2344    0.2354    5.158e-02      8 observações
Nile            50   0.1781    0.1315    3.356e-03    100 observações
```

O `Nile` REJEITA — e não tem sazonalidade nenhuma. Olhe o período: 100
observações, que é a série inteira. O ciclo não chega a se repetir uma segunda
vez dentro dos dados, e isso não é estação; é a tendência e a estrutura de baixa
frequência que sobraram depois de tirar a reta. Um pico sazonal se REPETE dentro
da série: poucas observações por ciclo e muitos ciclos, como os 12 do
`AirPassengers`. O primeiro período da grade, N, é justamente o contrário — um
ciclo só, que é tendência e não estação —, e é nele que o `Nile` caiu.

Em série de tamanho par a grade inclui ainda o último período, de duas
observações. Ele entra na soma do g como os outros; tirá-lo não muda a decisão
no `lh`, o caso mais apertado da tabela (o p vai de 0.052 para 0.062, e segue
sem rejeitar).

Por isso o bloco publica o PERÍODO junto com o veredito, e a `nota` avisa em voz
alta quando o pico não se repete ao menos duas vezes. Um "há periodicidade" lido
sem olhar o período é o erro mais fácil de cometer aqui.

### O período sai em observações

O periodograma mede frequência em ciclos por unidade de tempo, e o inverso disso
viria na unidade de tempo da série — num `AirPassengers` mensal, a estação de
doze meses apareceria como o número 1, que se lê como "um mês" e inverte o
sentido da frase. O bloco converte para OBSERVAÇÕES: o mesmo pico sai como 12,
que é o que se conta no gráfico. Em série de frequência 1 os dois números
coincidem. Junto vai quantas vezes o ciclo cabe na série — 12 no
`AirPassengers`, 1 no `Nile` —, que é a leitura já feita do período.

### O corte da dissertação e o p-valor

A dissertação decide comparando o **g** com o valor crítico zα = 1 - (α/n)^(1/(n-1))
(eq. 3.42), rejeitando quando o g passa de zα. Este bloco emite um p-valor de
verdade — o primeiro termo da série exata de Fisher — para que os três pontos do
card funcionem como em todo outro teste da coleção. As duas regras são A MESMA,
escrita de dois jeitos: nas cinco séries acima elas concordam, inclusive no caso
apertado do `lh`, onde o g fica logo ABAIXO de zα e o p-valor fica logo ACIMA de
5%, e as duas não rejeitam. Para que a comparação da dissertação possa ser
conferida direto no card, o zα sai publicado como o valor crítico a 5%, ao lado
do p-valor.

### Não precisa de estação declarada, mas precisa de tamanho

Ao contrário do `series/seasonality_kw`, este bloco ACEITA série de frequência 1:
ele não agrupa por estação, e o periodograma existe para qualquer série. Recusar
frequência 1 bloquearia justamente o uso para o qual ele serve — caçar um período
que ninguém declarou —, e foi assim que a linha do `Nile` da tabela acima foi
medida.

O que ele pede são oito observações. O limite vem da GRADE: com N observações o
periodograma só enxerga os períodos N, N/2, N/3 e assim por diante, e o menor
ciclo que esta coleção sabe declarar é o trimestral. Para que um período 4 exista
nessa grade e ainda se repita duas vezes dentro da série são precisas oito
observações — em N igual a 8 a grade tem o 4, em N igual a 7 ela não tem. Abaixo
disso o "período do pico" não é uma escolha entre alternativas: é o único lugar
onde ele poderia cair.

### O que sai

O g, o p-valor e a conclusão em palavras; o período do pico e o número de ciclos
vão em colunas extras, e o zα na coluna do valor crítico a 5%. A decisão é a 5%.
A cauda é a SUPERIOR: é o g grande — o pico que concentra a potência — que
derruba H0.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Procure periodicidade em uma série quando o ciclo não estiver especificado previamente. A frequência e o intervalo observado condicionam quais períodos podem ser detectados.

## Configuração

Nenhum. Uma entrada: **serie**.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("f", "series/periodicity_fisher", from = "pax")

tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("f", "series/periodicity_fisher", from = "nilo")
```

## Como interpretar

Um teste (`data/test`), com o período do pico e o número de ciclos em colunas
extras, e o zα da dissertação na coluna do valor crítico a 5%. Ligado numa
entrada de tabela, ele vira UMA linha de relatório: um `data/bind_rows` junta
vários testes num só quadro.

## Veja também

`series/seasonality_kw`, a outra pergunta da categoria — lá você declara as
estações e o teste as compara, aqui o teste procura o período sozinho;
`series/acf` e `series/seasonal_plot` para OLHAR o ciclo que o pico apontou antes
de acreditar nele; `series/subseries` quando o período achado bate com a
frequência da série; `series/diff` para tirar a tendência que produz o pico
enganoso do caso `Nile`.

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
