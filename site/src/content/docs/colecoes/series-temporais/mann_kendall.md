---
title: Mann-Kendall
description: "Mann-Kendall: a série tem tendência?"
section: colecoes
collection: series-temporais
node: series/mann_kendall
category: Tendência
order: 2
related: [series/f_tendencia, series/plot, series/runs]
---

## O que o bloco faz

Testa se a série tem TENDÊNCIA. É o teste de tendência mais usado em
climatologia — chuva, vazão, temperatura —, e o que se espera encontrar num
trabalho da área.

Ele conta, par a par, quantas vezes um valor posterior supera um anterior. Essa
contagem é o S: positivo quando o futuro costuma estar acima, negativo quando
costuma estar abaixo. A estatística Z é o S padronizado.

### Não paramétrico

Não supõe distribuição nenhuma para a série. É por isso que ele é o padrão onde
o dado não é normal, que é o caso da maior parte das variáveis ambientais. O
`series/f_tendencia` responde à mesma pergunta, mas cobra normalidade do erro em
troca; quando os dois concordam, a conclusão tem chão.

### A tendência é MONOTÔNICA

É o limite que mais surpreende: o teste procura movimento em um sentido só. Uma
série que sobe durante metade do período e desce na outra metade tem pares se
cancelando e pode sair SEM tendência nenhuma — o que não quer dizer que nada
aconteceu. Olhe o `series/plot` antes de acreditar num "não há evidência".

### A direção vem no sinal de Z

Rejeitar H0 não é só "há tendência": Z positivo é tendência de AUMENTO, Z
negativo é tendência de QUEDA, e é assim que a conclusão sai escrita.

### Empates

Valor repetido não aponta direção nenhuma, e a variância é corrigida por isso.
Sem a correção, uma série com muitos valores iguais — medição arredondada, uma
corrida de zeros na chuva — sairia com um p-valor otimista demais. A `nota` diz
quantos grupos de empate entraram na conta, junto do S.

### O que sai

A estatística Z, o p-valor bilateral e a conclusão em palavras; o S vai na
coluna extra do relatório. A decisão é a 5%. O p-valor vem da aproximação
normal, que pede pelo menos dez observações — abaixo disso o bloco recusa em vez
de devolver um número que a amostra não sustenta. Dez bastam porque a
aproximação aqui corre sobre TODAS as n(n-1)/2 comparações par a par, e não
sobre uma contagem de símbolos: dez observações já rendem quarenta e cinco
comparações, enquanto o `series/runs`, que depende do corte pela mediana, só
alcança a normal dele com quarenta observações.

### Série autocorrelacionada: a **Correção**

O teste supõe observações independentes, e série ambiental quase nunca é:
com autocorrelação positiva o S varia mais do que a fórmula diz, e o teste
rejeita bem acima dos 5% nominais. Duas correções publicadas:

- **nenhuma** — o teste de Mann (1945), como na dissertação. É o padrão.
- **hamed_rao** — Hamed & Rao (1998): o mesmo S, com a variância multiplicada
  por n/n*, calculado das autocorrelações dos POSTOS da série sem a tendência
  de Sen, só as significativas a 5%. A `nota` traz o n/n*: acima de 1, a
  autocorrelação alargou a variância e o Z encolheu.
- **pre_branqueamento** — Yue et al. (2002), o pré-branqueamento livre de
  tendência: tira a tendência de Sen, remove o AR(1) do resto pelo r1, devolve
  a tendência e testa a série resultante (uma observação a menos; pede 11).
  A `nota` traz o r1. Como no artigo (passos 1 a 4, p. 1822-1823), o AR(1) é
  removido sempre, significativo ou não; o r1 é o do `acf` (o do
  `modifiedmk`), n/(n − 1) vezes menor que o da eq. 14a do artigo.
- **bootstrap_blocos** — o mesmo S, com o p-valor de um bootstrap de blocos
  móveis (Kundzewicz & Robson, 2004): a série é cortada em blocos de
  round(√n) observações seguidas, sorteados com reposição e emendados, 1999
  vezes; o p é a fração das reamostras com |S*| ≥ |S|. Os blocos guardam a
  dependência de curto alcance e desmancham a tendência. Usa a semente do nó:
  o mesmo fluxo dá o mesmo p. A `nota` traz o tamanho do bloco.

No Nilo (`datasets::Nile`, 100 anos):

```
correção            Z        p-valor     nota
nenhuma           -4,128    3,7e-05     S = -1387
hamed_rao         -2,820    0,0048      n/n* = 2,143
pre_branqueamento -4,577    4,7e-06     r1 = 0,375, n = 99
```

As duas primeiras seguem o `modifiedmk` (`mmkh` e `tfpwmk`), conferidas contra ele.
Nenhuma das duas devolve o nível nominal. Medido em série SEM tendência, erro AR(1),
2000 réplicas, rejeição a 5%:

```
phi   n     nenhuma   hamed_rao   pre_branqueamento
0     60      5,1%       8,5%          4,7%
0,3   60     13,5%      15,3%         15,6%
0,6   60     30,7%      21,1%         39,4%
0,6   120    33,4%      18,0%         43,1%
```

O Hamed-Rao reduz o excesso quando a autocorrelação é forte, mas não o
elimina e custa um pouco em ruído branco; o pré-branqueamento livre de
tendência PIORA o nível, como Hamed (2009) já apontava — a tendência de Sen
estimada na série autocorrelacionada volta somada. Use-o para reproduzir um
trabalho que o aplicou, não como remédio. Em raros casos (até 1% das réplicas
acima) a soma do Hamed-Rao sai negativa e o bloco recusa em vez de devolver
NaN.

O `bootstrap_blocos` é a correção que mais se aproxima do nível, e ainda
assim não o alcança com autocorrelação forte. Medido em série SEM tendência,
AR(1), 1000 réplicas por caso (erro de Monte Carlo de 0,7 a 0,9 ponto),
rejeição a 5%:

```
phi   n     nenhuma   bootstrap_blocos
0.3   60     16.0%        7.2%
0.3   120    13.6%        5.5%
0.6   60     31.0%        9.0%
0.6   120    31.2%        7.7%
```

Com autocorrelação moderada e série de uns cem pontos ele devolve o nível;
com phi de seis décimos, reduz o excesso de 31% para 8% a 9%, sem zerá-lo. O
preço é poder: com uma tendência de 1,8 desvio do ruído ao longo da série, ele
detecta em 77% (phi 0,3, n = 60), 96% (phi 0,3, n = 120), 43% (phi 0,6, n =
60) e 66% (phi 0,6, n = 120) das vezes — menos que o teste sem correção, cujo
poder aparente vem em parte do nível inflado. A regra de bloco do
`modifiedmk::bbsmk` (autocorrelações significativas seguidas, mais um) dá
blocos de 3 a 4 e rejeitou 17% a 19% com phi 0,6 (300 réplicas); por isso o
bloco aqui é √n. Com autocorrelação forte, prefira modelar o erro
(`series/regression` com **Erro** = `arma` e o `series/f_tendencia`).

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Teste se há tendência monotônica ao longo do tempo sem exigir uma forma linear. O teste avalia ordenação, não o tamanho da mudança.

## Configuração

- **Correção para autocorrelação** — `nenhuma` (padrão), `hamed_rao`,
  `pre_branqueamento` ou `bootstrap_blocos`.

Uma entrada: **serie**.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("mk", "series/mann_kendall", from = "nilo") |>
  tr_add("mk_hr", "series/mann_kendall", correcao = "hamed_rao", from = "nilo")
```

## Como interpretar

Um teste (`series/test`), com o S numa coluna extra. Ligado numa entrada de
tabela, ele vira UMA linha de relatório: um `data/bind_rows` junta vários testes
num só quadro.

## Veja também

`series/f_tendencia`, a mesma pergunta pela regressão; `series/adf` e
`series/kpss`, que perguntam por estacionariedade e não por tendência;
`series/plot` para ver se o movimento é mesmo de um sentido só;
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
