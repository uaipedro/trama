---
title: Run
description: "Run (Wald-Wolfowitz): a série é aleatória?"
section: colecoes
collection: series-temporais
node: series/runs
category: Tendência
order: 2
related: [series/mann_kendall, series/interpolate, series/window]
---

## O que o bloco faz

Testa se a série foi gerada ao ACASO. Também chamado teste de sequências de
Wald-Wolfowitz: marca cada valor conforme esteja acima ou abaixo da mediana e
conta as SEQUÊNCIAS — trechos seguidos do mesmo lado. Uma série aleatória troca
de lado com uma frequência previsível; sequências de menos dizem que os valores
se agrupam, sequências demais, que alternam.

### A hipótese nula é ALEATORIEDADE, e não ausência de tendência

É a diferença que mais importa nesta página, e o que separa este bloco dos dois
vizinhos de categoria. Rejeitar aqui NÃO é concluir que há tendência: é concluir
que a série não parece ter saído do acaso. Tendência é só uma das causas que
produzem sequências de menos — uma mudança de nível no meio da série, um ciclo e
qualquer agrupamento produzem o mesmo efeito, e o teste não distingue os três. A
conclusão sai escrita assim, sem afirmar tendência, justamente para não dar por
respondida uma pergunta que este bloco não fez.

### Não use este teste sozinho

A literatura já documentou o erro. Back (2001), revisado pela dissertação que
esta coleção segue, aplicou Run, Pettitt e Mann-Kendall às mesmas séries
climáticas: o Run foi o ÚNICO que deixou de detectar a tendência que os outros
dois encontraram. Morettin & Toloi (2006) colocam Run e Cox-Stuart na mesma
posição — testes para rodar ao lado de outros, não no lugar deles. Quem procura
tendência deve ligar também um `series/mann_kendall`; um "não rejeita H0" daqui,
sozinho, é evidência fraca de que não há nada acontecendo.

### Empates com a mediana

Valor exatamente IGUAL à mediana não fica nem acima nem abaixo, e sai da conta,
como manda o método. Empurrá-lo para um dos lados inventaria um símbolo que o
dado não deu e ainda emendaria duas sequências numa só. O descarte muda o número
de observações que entram no teste e não aparece em lugar nenhum do card, então a
`nota` diz quantas saíram e quantas sobraram de cada lado.

### O que sai

A estatística Z, o p-valor bilateral e a conclusão em palavras; o número de
sequências vai na coluna extra do relatório. A decisão é a 5%. O p-valor vem da
aproximação normal, e ela pede pelo menos 20 observações de cada lado da mediana
— por isso o bloco exige 40 observações, que é o tamanho em que o corte pela
mediana entrega esses 20 de cada lado. Abaixo disso valeria a distribuição exata
do número de sequências, que este bloco não calcula, e devolver mesmo assim o Z
seria publicar uma precisão que a amostra não sustenta. Uma série que passe no
tamanho mas chegue ao teste com um platô na mediana também é recusada, pelo mesmo
motivo.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Teste a aleatoriedade da sequência por meio da alternância de sinais em torno da mediana. Agrupamentos ou alternâncias excessivas podem indicar estrutura temporal.

## Configuração

Nenhum. Uma entrada: **serie**.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("run", "series/runs", from = "nilo")
```

## Como interpretar

Um teste (`series/test`), com o número de sequências numa coluna extra. Ligado
numa entrada de tabela, ele vira UMA linha de relatório: um `data/bind_rows`
junta vários testes num só quadro.

## Veja também

`series/mann_kendall` e `series/cox_stuart`, que perguntam por tendência de
verdade — a dissertação compara os três, e este é o que menos detecta;
`series/pettitt`, o terceiro teste que Back (2001) aplicou às mesmas séries, e
que localiza a mudança em vez de afirmar tendência; `series/plot` para ver de que
jeito a aleatoriedade falhou; `series/ljung_box`, que também pergunta se a série
é ruído, mas pela autocorrelação.

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
