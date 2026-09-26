---
title: Pettitt
description: "Pettitt: a série tem um ponto de mudança?"
section: colecoes
collection: series-temporais
node: series/pettitt
category: Tendência
order: 2
related: [series/mann_kendall, series/cox_stuart, series/interpolate]
---

## O que o bloco faz

Testa se a série tem um PONTO DE MUDANÇA: um instante a partir do qual ela passou
a se comportar como outra série. A hipótese nula é a HOMOGENEIDADE — que o trecho
antes e o trecho depois de qualquer ponto venham da mesma população. O teste
percorre todos os cortes possíveis, mede em cada um o quanto as duas partes se
separam, e fica com o corte onde a separação é maior.

### Quebra não é tendência

É o que mais importa nesta página, e o que separa este bloco dos três vizinhos de
categoria. Rejeitar aqui NÃO é concluir que a série sobe ou que a série desce: é
concluir que houve uma RUPTURA, e dizer onde. A dissertação que esta coleção
segue é enfática no ponto, citando Niel et al. (1998) — o teste localiza a
mudança, não afirma direção nenhuma. A conclusão sai escrita assim: nomeia a
observação em que a série mudou e cala sobre direção, para não dar por respondida
uma pergunta que este bloco não fez. Uma série pode mudar de nível para baixo no
meio e ainda assim subir do começo ao fim.

### Use junto com o Mann-Kendall

É o par que a dissertação recomenda, e Penereiro & Orlando (2013), citados lá,
usam os dois exatamente assim: o `series/mann_kendall` estabelece que existe
tendência, o Pettitt localiza QUANDO a série mudou. Um responde "o quê", o outro
"quando", e nenhum dos dois responde pelo outro. Ligar só este bloco e escrever
"há tendência" no relatório é o erro que esta página inteira existe para evitar.

### Onde foi a mudança

Rejeitando H0, o ponto de mudança sai em dois formatos, e os dois vão para o
relatório: a POSIÇÃO na série, que é o número que outros pacotes reportam e que
se confere, e o RÓTULO do período — "1953 jun" numa mensal, "1960 T3" numa
trimestral, o ano numa anual. O rótulo vem do calendário da própria série; numa
série sem calendário ele seria o próprio índice, e aí o bloco não o repete.

Não rejeitando, a posição continua sendo calculada e publicada: é onde o corte
foi MAIS favorável, e nem assim deu. Ler esse número como uma quebra fraca é lê-lo
errado.

### O que sai

A estatística K, o p-valor bilateral e a conclusão em palavras; a posição do
ponto de mudança e o rótulo do período vão nas colunas extras do relatório. A
decisão é a 5%. O p-valor é uma APROXIMAÇÃO, e ela passa de 1 em série curta e
homogênea — nesse caso o valor é preso em 1 e a `nota` avisa, porque um "p = 1"
no card se lê como certeza de homogeneidade quando é só a fórmula estourando. O
bloco pede pelo menos onze observações: com dez, o maior K que existe — o da
série perfeitamente monotônica — ainda fica acima do corte de 5%, e o teste não
teria como rejeitar nunca, qualquer que fosse o dado. É o mesmo tipo de piso
medido do `series/mann_kendall` e do `series/cox_stuart`, e por isso os números
diferem entre os quatro blocos: cada um sai da fórmula do seu próprio teste, e
não de uma convenção comum.

### O ponto pode empatar

Mais de um corte pode alcançar exatamente o mesmo K máximo, e o reportado é o
PRIMEIRO deles. Quando isso acontece a `nota` diz quantos empataram: são cortes
igualmente bons, e ler o número publicado como o único ponto possível seria ler
mais do que o teste disse.

### Série autocorrelacionada: **Correção**

Autocorrelação positiva imita ponto de mudança: sem correção, em série
homogênea com AR(1) de seis décimos o teste rejeita em metade das vezes. Com
**Correção** = `bootstrap_blocos`, o p-valor sai de um bootstrap de blocos
móveis (Kundzewicz & Robson, 2004): blocos de round(√n) observações seguidas,
sorteados com reposição e emendados, 1999 vezes; o p é a fração das
reamostras com K* ≥ K. O ponto de mudança e o K não mudam. Usa a semente do
nó. Medido em série homogênea, AR(1), 1000 réplicas por caso, rejeição a 5%:

```
phi   n     nenhuma   bootstrap_blocos
0.3   60     16.5%        3.5%
0.3   120    18.1%        4.6%
0.6   60     45.5%        8.7%
0.6   120    54.8%        7.8%
```

Com autocorrelação moderada o nível é o nominal; com phi de seis décimos
fica em 8% a 9%, longe dos 50% sem correção mas acima dos 5% — um "rejeita"
apertado aí pede cautela. Poder, com um degrau de 1.5 no meio da série: 89%
(phi 0.3, n = 60), 100% (0.3, 120), 59% (0.6, 60) e 86% (0.6, 120).

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Localize uma possível mudança de nível em um único ponto da série. O teste identifica uma quebra candidata; contexto e inspeção determinam se ela representa uma mudança real do processo.

## Configuração

**correcao** — `nenhuma` (padrão) ou `bootstrap_blocos` (p por bootstrap de
blocos móveis, para série autocorrelacionada). Uma entrada: **serie**.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("pet", "series/pettitt", from = "nilo")
```

## Como interpretar

Um teste (`series/test`), com a posição do ponto de mudança e o rótulo do período
em colunas extras. Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta vários testes num só quadro.

## Veja também

`series/mann_kendall`, o par recomendado — ele diz se há tendência, este diz
quando a série mudou; `series/cox_stuart` e `series/runs`, os outros dois não
paramétricos da categoria; `series/plot` para ver a quebra que o teste apontou;
`series/window` para analisar separadamente os dois trechos que ele separou.

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
