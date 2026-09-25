---
title: Cox-Stuart
description: "Cox-Stuart: a série tem tendência?"
section: colecoes
collection: series-temporais
node: series/cox_stuart
category: Tendência
order: 2
related: [series/mann_kendall, series/interpolate, series/window]
---

## O que o bloco faz

Testa se a série tem TENDÊNCIA por um teste de SINAL: pareia observações
distantes no tempo e conta quantas vezes a segunda é maior que a primeira. Se
não houvesse tendência, subir e descer seriam igualmente prováveis, e essa
contagem — o M — se comportaria como cara-ou-coroa. É o mesmo veredito do
`series/mann_kendall` por um caminho mais barato, e a dissertação que esta
coleção segue compara os dois diretamente.

### As duas formas de parear

O param **pareamento** decide QUAIS observações formam cada par, e a escolha não
é cosmética: os dois caminhos dão estatísticas diferentes na mesma série.

**Terços** é o padrão, e é o teste original de Cox & Stuart: compara o primeiro
terço da série com o último e JOGA FORA o miolo. Descartar o meio é o que dá
poder ao teste contra tendência monotônica — são os extremos que acumularam o
movimento, e parear observações vizinhas dilui o contraste.

Não espere daqui o mesmo número do pacote `trend`, que responde à mesma
pergunta por outra conta: ele padroniza pelo tamanho da SÉRIE (n/6 e n/12) em
vez de pelos pares que de fato sobraram, corrige continuidade em série curta,
nunca usa a binomial exata, e reporta a estatística sem sinal. Os dois só
coincidem num conjunto estreito de séries, e esse limite está fixado em teste
em vez de prometido aqui — três tentativas de escrevê-lo como regra saíram
erradas, porque ele tem mais condições do que cabe numa frase. O sinal, aqui,
fica de propósito: é ele que deixa a conclusão dizer "queda" em vez de só
"tendência".

**Metades** é a formulação da dissertação: parte a série ao meio e pareia cada
observação com a que está meia série adiante. Escolha esta quando o trabalho
tiver de reproduzir a dissertação. Entram mais pares, cada um com contraste
menor, e o resultado muda de verdade: na mesma reta com ruído, o Z em metades
sai maior e o p-valor duas ordens de grandeza menor que o de terços. Mesma
série, mesma hipótese nula, dois números.

### Empates

Par com os dois valores iguais não aponta direção nenhuma e é DESCARTADO, como
manda o método — contá-lo como "não subiu" seria pôr no prato da queda um par
que não disse nada. A `nota` diz quantos pares sobraram, que é o número que
explica um M pequeno numa série grande.

### Exata ou aproximada, conforme o número de pares

Com poucos pares o p-valor vem da binomial EXATA; de vinte pares em diante, da
aproximação normal, e aí a estatística é o Z. No ramo exato não há Z nenhum
calculado, e o que o card mostra é o próprio M, com esse rótulo: reportar um Z
que ninguém computou seria mentir sobre a conta. A `nota` diz sempre qual dos
dois caminhos entrou, porque a escolha é automática.

### A direção vem da contagem

Rejeitar H0 não é só "há tendência": mais pares subindo que descendo é tendência
de AUMENTO, o contrário é de QUEDA, e é assim que a conclusão sai escrita.

### O que sai

A estatística (Z ou M), o p-valor bilateral e a conclusão em palavras; o M e o
número de pares vão nas colunas extras do relatório. A decisão é a 5%. O bloco
pede pelo menos dezesseis observações: abaixo disso o terço da ponta fica com
menos de seis pares, e com menos de seis pares nem o resultado mais extremo
possível alcança o corte de 5% — o teste não teria como rejeitar nunca.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Teste uma tendência monotônica comparando pares de observações ao longo do tempo. O teste usa sinais e não exige que a tendência seja linear.

## Configuração

**pareamento** — quais observações formam cada par. *Terços* (padrão) compara o
primeiro terço com o último, descartando o miolo, como no artigo original;
*metades* pareia cada observação com a que está meia série adiante, como na
dissertação. Uma entrada: **serie**.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("cs", "series/cox_stuart", from = "nilo")
```

## Como interpretar

Um teste (`series/test`), com o M e o número de pares em colunas extras. Ligado
numa entrada de tabela, ele vira UMA linha de relatório: um `data/bind_rows`
junta vários testes num só quadro.

## Veja também

`series/mann_kendall`, a mesma pergunta contando todos os pares — a dissertação
compara os dois, e vale rodar os dois; `series/f_trend`, a mesma pergunta
pela regressão; `series/plot` para ver se o movimento é mesmo de um sentido só;
`series/example` para uma série com tendência à mão.

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
