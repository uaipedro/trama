---
title: Kruskal-Wallis
description: "Kruskal-Wallis: a série tem sazonalidade?"
section: colecoes
collection: series-temporais
node: series/kruskal_wallis
category: Tendência
order: 2
related: [series/f_sazonal, series/diff, series/transform]
---

## O que o bloco faz

Testa se a série tem SAZONALIDADE sem supor distribuição nenhuma. Põe todas as
observações em POSTOS — o menor valor da série inteira vira posto 1, o maior
vira o posto N — e compara a soma dos postos de cada estação. Se janeiro é
sempre alto e julho é sempre baixo, as somas se afastam e o H cresce. H0 é "as
estações têm a mesma distribuição" — sem sazonalidade —, e rejeitar é concluir
que há.

É o irmão não paramétrico do `series/f_sazonal`, que responde à mesma pergunta
pedindo erro normal em troca.

### Sazonalidade determinística

O que este teste enxerga é o padrão que se repete IGUAL todo ciclo: janeiro
sempre alto, julho sempre baixo. A sazonalidade ESTOCÁSTICA — a que vai mudando
de ano para ano — não tem um "nível de janeiro" fixo para o posto encontrar, e
passa por aqui sem ser vista. A dissertação separa as duas na seção 3.4: para a
estocástica o caminho é a diferença sazonal do `series/diff`.

### Tendência atrapalha, e o log não salva

É o ponto que mais engana desta página, e ele se mede no caso de livro. O
`AirPassengers` é A série sazonal dos manuais, e ela NÃO rejeita aqui:

```
Kruskal-Wallis chi-squared = 11.148, df = 11, p-value = 0.4309
```

Não é defeito do bloco: é a TENDÊNCIA. A série quase triplica ao longo dos doze
anos, então os postos altos são todos dos anos finais e se espalham por todas as
estações — cada mês recebe um valor baixo do começo da série e um alto do fim, e
as somas de postos acabam empatando. O padrão sazonal existe; o posto o perde
por baixo do crescimento.

Passar um log ANTES não muda NADA, e isso costuma surpreender: o log torna a
sazonalidade multiplicativa em aditiva, sim, mas este teste é de POSTOS e o log
é monotônico — ele não troca a ordem de valor nenhum, então o H e o p-valor saem
idênticos aos de cima, dígito por dígito. O `series/transform` serve para
estabilizar a variância; para este teste ele é inócuo.

O que resolve é tirar a TENDÊNCIA. Uma diferença simples do `series/diff` antes
do bloco, e o mesmo `AirPassengers` rejeita com folga:

```
Kruskal-Wallis chi-squared = 119.2, df = 11, p-value = 2.623e-20
```

Ou seja: um "não rejeita H0" numa série que você SABE que é sazonal é quase
sempre a tendência falando. Olhe a série no `series/seasonal_plot` ou no
`series/subseries` antes de concluir que não há estação.

### A série precisa ter estação

Série com frequência 1 é cartão vermelho, e não um card verde dizendo que não há
sazonalidade: sem ciclo declarado não existem estações para comparar, e o teste
seria mandado comparar um grupo só. Declare a frequência no nó que cria a série.
Menos de TRÊS ciclos completos também é recusado, e não dois como nos outros
blocos sazonais. Com duas observações por estação, o maior H possível — as
estações perfeitamente separadas — não chega ao valor crítico, então o teste não
teria como rejeitar e o veredito estaria decidido antes de ler o dado:

```
frequência   ciclos   maior H possível   corte a 5%
4            2          6.667              7.815
4            3         10.385              7.815
2            2          2.400              3.841
2            3          3.857              3.841
12           2         22.880             19.675
```

A mensal até rejeitaria com dois ciclos, mas o piso é o mesmo para todas as
frequências: uma regra só, ao custo de um ano de dado.

### O que sai

A estatística H, o p-valor e a conclusão em palavras; os graus de liberdade — o
número de estações menos um — vão numa coluna extra, porque é com eles que se lê
o H contra a tabela do qui-quadrado. A decisão é a 5%. A cauda é a SUPERIOR: é o
H grande que derruba H0, ao contrário dos testes de tendência da coleção. A
`nota` diz quantas estações entraram e quantas observações cada uma tem — ciclo
incompleto na ponta deixa umas com uma observação a mais, e é o que explica um H
diferente do da mesma série fechada no último ciclo.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue um
`series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Compare a distribuição das observações entre posições do ciclo para verificar sazonalidade. A comparação é não paramétrica e depende da frequência da série.

## Configuração

Nenhum. Uma entrada: **serie**, que precisa ter frequência maior que 1.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("kw", "series/kruskal_wallis", from = "pax")

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("d", "series/diff", from = "pax") |>
  tr_add("kw", "series/kruskal_wallis", from = "d")
```

## Como interpretar

Um teste (`series/test`), com os graus de liberdade numa coluna extra. Ligado
numa entrada de tabela, ele vira UMA linha de relatório: um `data/bind_rows`
junta vários testes num só quadro.

## Veja também

`series/f_sazonal`, a mesma pergunta pedindo erro normal em troca;
`series/diff` para tirar a tendência antes do teste, ou para a diferença sazonal
quando a sazonalidade é estocástica; `series/transform`, o log que estabiliza a
variância e que NÃO muda este teste; `series/seasonal_plot` e `series/subseries`
para ver a sazonalidade que o teste mede.

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
