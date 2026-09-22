---
title: ADF
description: "Dickey-Fuller Aumentado: a série tem raiz unitária?"
section: colecoes
collection: series-temporais
node: series/adf
category: Raiz unitária
order: 2
related: [series/interpolate, series/window, series/kpss]
---

## O que o bloco faz

Testa se a série tem RAIZ UNITÁRIA — a não estacionariedade que faz o nível
passear sem voltar, e que se resolve diferenciando.

### A leitura é invertida

A hipótese nula aqui é a RAIZ UNITÁRIA. Quem está acostumado a "p-valor
pequeno quer dizer que achei alguma coisa" lê este teste ao contrário:
rejeitar H0 é concluir ESTACIONÁRIA.

Não rejeitar não prova raiz unitária — com amostra pequena o teste deixa de
rejeitar por falta de poder. Por isso o ADF anda em par com um teste de
hipótese nula oposta (H0 = estacionária): é quando os dois lados concordam que
a conclusão tem chão. Se discordam, a série está no limite, e diferenciar é o
lado seguro.

### O que sai

A estatística t, a tabela de valores críticos a 10%, 5% e 1% e a conclusão em
palavras. O `urca` só publica a tabela: o `p_valor` sai em branco, e não
interpolado a partir dela — seria inventar precisão que o pacote não dá. A
decisão a 5% vem do valor crítico.

**Termos determinísticos**: `constante` para série que oscila em torno de um
nível; `tendência` para série que pode ser estacionária em torno de uma reta.

**Defasagens**: o teto do termo aumentado. `0` usa a regra de sempre (a raiz
cúbica de n - 1); dentro do teto, quem escolhe é o AIC.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Avalie a hipótese de raiz unitária quando decidir se a série precisa de diferenças. A conclusão depende dos termos determinísticos escolhidos e deve ser lida junto ao KPSS ou ao gráfico.

## Configuração

- **Termos determinísticos** — `constante` ou `tendência`.
- **Defasagens** — teto de defasagens; `0` para a regra automática.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d", "series/diff", from = "log") |>
  tr_add("adf", "series/adf", from = "d")
```

## Como interpretar

Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta vários testes num só.

## Veja também

`series/kpss`, o teste de hipótese nula oposta, que se lê ao lado deste;
`series/phillips_perron`, a mesma hipótese nula por outro caminho;
`series/zivot_andrews` quando a série pode ter uma QUEBRA: este bloco aqui
tende a não rejeitar nesse caso, e lá a quebra é estimada e o veredito muda;
`series/ndiffs` para saber quantas diferenças a série pede; `series/diff` para
fazê-las.

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
