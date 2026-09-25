---
title: Zivot-Andrews
description: "Zivot-Andrews: raiz unitária, com a quebra achada pelo próprio teste?"
section: colecoes
collection: series-temporais
node: series/zivot_andrews
category: Raiz unitária
order: 2
related: [series/adf, series/phillips_perron, series/plot]
---

## O que o bloco faz

Testa se a série tem RAIZ UNITÁRIA admitindo que ela possa ter sofrido uma
QUEBRA ESTRUTURAL — e achando a data da quebra sozinho.

### Por que este bloco existe

É a resposta a um viés dos vizinhos, e é o motivo de a dissertação trazer o
teste. O `series/adf` e o `series/phillips_perron` tendem a NÃO REJEITAR quando
a série tem quebra estrutural (MARGARIDO, 2001): uma série que é estacionária
em torno de uma média que DESLOCOU no meio do caminho se parece muito com um
passeio aleatório para os dois, e eles não rejeitam a raiz unitária.

Então a leitura prática é esta: quando o `series/adf` não rejeita
numa série em que você desconfia de uma ruptura, este é o bloco que confere se
a quebra era a explicação.

Medido, numa série de 120 observações feita de 60 pontos em torno de zero e 60
em torno de seis — estacionária dos dois lados, com um degrau no meio:

```
bloco                     estatística   decisão a 5%   conclusão
series/adf                     -1.57    não rejeita    não há evidência contra a raiz unitária
series/zivot_andrews          -14.21    rejeita        estacionária, quebra na obs. 60
```

O ADF erra a pergunta inteira; este acha o degrau na observação exata em que
ele foi posto.

### A quebra é ESTIMADA, não informada

Não existe parâmetro de data, e é de propósito: o teste percorre os cortes
entre 15% e 85% da série, ajusta a regressão em cada um e fica com aquele que
MAIS favorece a estacionariedade. A data sai como resultado, não como entrada — é
o que separa este teste de um ADF com dummy escrita à mão.

A janela segue o teste publicado por Zivot e Andrews, de onde vem a tabela de
valores críticos, e não o `urca`, que varre todos os cortes. Sem ela a quebra
apontada às vezes cai na ponta da série, com um trecho de uma observação só, e
essa data não se lê como quebra de nada. Aparar não muda de forma sensível o
quanto o teste rejeita: ela conserta a data, não o excesso de rejeição da série
curta descrito mais abaixo.

A contrapartida é que, escolhendo o corte mais favorável entre muitos, ele
precisa de valores críticos bem mais severos que os do ADF. São os da tabela
abaixo, e é por isso que um t de -4.5 que rejeitaria no ADF não rejeita aqui.

### A leitura é invertida

Como no ADF, H0 é a RAIZ UNITÁRIA, e sem quebra — a quebra só entra na
alternativa (eq. 3.35 a 3.37 da dissertação): rejeitar é concluir ESTACIONÁRIA
(em torno de um nível que quebrou). A cauda é a de BAIXO — o t bem negativo é o que
derruba H0.

E não rejeitar aqui diz MAIS do que não rejeitar no ADF: a explicação
alternativa mais comum, a quebra, já foi dada de graça à série e mesmo assim
não bastou. Por isso a conclusão sai escrita "mesmo admitindo uma quebra".

### Cada modelo tem a SUA tabela

O que você declara em **O que quebra** não muda só a regressão: muda os valores
críticos junto. São três tabelas diferentes, e o bloco lê a do modelo que rodou.

```
o que quebra        1%      5%     10%     equação
nível            -5.34   -4.80   -4.58     3.35
inclinação       -4.93   -4.42   -4.11     3.36
nível e inclinação -5.57  -5.08   -4.82     3.37
```

A Tabela 3.2 da dissertação publica só a última linha, que é a do modelo
completo — o default daqui. Escolha `nível` para um degrau (a série pula e
segue no mesmo ritmo), `inclinação` para uma virada de tendência (a série muda
de inclinação sem pular), `ambas` quando não souber ou quando as duas coisas
mudarem. No exemplo do degrau acima, `inclinação` sai com t de -2.68 e NÃO
rejeita: o modelo errado não enxerga a quebra certa.

### O que sai

A estatística t, a tabela de valores críticos a 10%, 5% e 1% e a conclusão em
palavras; a posição da quebra e o rótulo do período vão nas colunas extras. O
`urca` publica só a tabela, então o `p_valor` sai em branco — interpolar um
p-valor a partir dela seria inventar precisão que o pacote não dá. A decisão a
5% vem do valor crítico.

**Defasagens**: quantas diferenças defasadas entram na regressão, e como se
chega ao número — é o que limpa a autocorrelação do erro, e a tabela de
críticos supõe que ela foi limpa.

- **Escolha das defasagens = t_sig** (padrão) — a regra do artigo: do geral
  para o específico (Perron, 1989; Zivot & Andrews, 1992), EM CADA CORTE. Para
  cada data candidata, parte do teto e, enquanto o t da ÚLTIMA diferença
  defasada não for significativo a 10% (|t| < 1.645), tira uma; o t da raiz
  unitária daquele corte é o da regressão com o número que sobrou, e o teste é
  o menor t entre os cortes. **Defasagens** é o teto; `0` usa a regra de
  Schwert (1989), trunc(12·(n/100)^(1/4)), limitada ao que a série comporta. A
  `nota` diz quantas ficaram no corte vencedor e qual foi o teto. (Na versão 2
  o corte era escolhido primeiro e o número depois, só nele; a versão 3 segue
  o artigo.)
- **fixa** — o número vale como foi dado, sem busca, e `0` é zero defasagens
  (na versão 2, `0` caía na raiz cúbica de n - 1).

O preço da regra do artigo é o nível em amostra finita: a busca em cada corte
escolhe, entre muitos k, o que mais favorece a rejeição. Medido sob passeio
aleatório (modelo de nível, 300 réplicas), `t_sig` rejeita a 5% em 31% das
vezes com 30 observações, 27% com 50 e 13% com 100; `fixa` com a raiz cúbica
de n - 1 defasagens, em 10%, 6% e 5%. Abaixo de 100 observações a `nota`
avisa, e o resultado deve ser conferido com `fixa`.

Um teto (ou número fixo) grande demais para uma série curta deixa a regressão
da quebra sem graus de liberdade, e nesse caso o bloco recusa dizendo qual é o
máximo — sem isso o `urca` morreria com um erro cru do R.

Com k = 8 fixo, no PNB de Nelson e Plosser (1909-1970, em log,
`urca::nporg`), modelo de nível, o bloco dá t = -5.576 (real) e -5.824
(nominal), quebra em 1929 — iguais ao `urca::ur.za`, e aos -5.58 e -5.82
citados de Zivot e Andrews (1992), que não foram conferidos no PDF do artigo.

### Precisa de série, e de série que chegue

O bloco pede pelo menos 20 observações, e o piso é mais alto que os 12 dos
irmãos de categoria porque este teste gasta mais: a dummy da quebra (duas, no
modelo completo) sai dos mesmos graus de liberdade, e ainda se escolhe o melhor
entre n - 1 cortes.

O número foi medido. Sob passeio aleatório — onde H0 é VERDADEIRA e toda
rejeição é erro — o teste rejeita a 5% em 68% das amostras com n = 11, 24% com
n = 14, 18% com n = 18, contra 14% com n = 20. Abaixo de vinte o bloco
devolveria "estacionária com quebra" para série com raiz unitária de verdade na
maioria das vezes.

O excesso não acaba em vinte, só deixa de ser catastrófico: a taxa fica em torno
de 10% a 14% até n = 30, e mesmo depois não chega aos 5% nominais: medido no
modelo completo, fica em torno de 7% a 10% com n = 40 e perto de 7% com n = 100.
Entre vinte e quarenta, onde o excesso é maior, a `nota` do teste avisa, e um "rejeita H0" apertado aí pede
confirmação — de preferência olhando a série no `series/plot`, para ver se a
quebra que ele apontou existe.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Teste raiz unitária permitindo uma quebra estrutural única cuja posição é estimada pelo próprio procedimento. Compare a hipótese de quebra com evidências históricas e gráficas.

## Configuração

- **O que quebra** — `nível`, `inclinação` ou `ambas`; muda a regressão e a
  tabela de valores críticos junto.
- **Defasagens** — com `t_sig`, o teto da busca (`0` = regra de Schwert);
  com `fixa`, o número usado (`0` = nenhuma).
- **Escolha das defasagens** — `t_sig` (padrão) ou `fixa`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("za", "series/zivot_andrews", from = "nilo") |>
  tr_add("adf", "series/adf", from = "nilo")
```

## Como interpretar

Um teste (`series/test`), com a posição da quebra e o rótulo do período em
colunas extras. Ligado numa entrada de tabela, ele vira UMA linha de relatório:
um `data/bind_rows` põe este e o `series/adf` lado a lado, que é como a
diferença entre os dois se lê.

## Veja também

`series/adf`, o teste que este corrige — é a discordância entre os dois que
diz que havia uma quebra; `series/kpss`, de hipótese nula oposta;
`series/pettitt`, que localiza um ponto de mudança sem supor modelo nenhum — os
dois respondem perguntas vizinhas, e a dissertação usa localização de quebra e
teste de tendência juntos; `series/plot` para OLHAR a quebra que o teste
apontou antes de acreditar nela.

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
