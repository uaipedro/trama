---
title: Phillips-Perron
description: "Phillips-Perron: a série tem raiz unitária?"
section: colecoes
collection: series-temporais
node: series/phillips_perron
category: Raiz unitária
order: 2
related: [series/adf, series/kpss, series/interpolate]
---

## O que o bloco faz

Testa se a série tem RAIZ UNITÁRIA, a mesma hipótese nula do `series/adf`.
Muda o caminho: em vez de acrescentar defasagens à regressão para limpar a
autocorrelação dos resíduos, o Phillips-Perron corrige a própria estatística.
Por isso não tem o parâmetro de defasagens do ADF.

### A leitura é invertida

Rejeitar H0 é concluir ESTACIONÁRIA. E, como no ADF, não rejeitar não prova
raiz unitária: com amostra pequena o teste deixa de rejeitar por falta de
poder. O par natural é o `series/kpss`, de hipótese nula oposta (H0 =
estacionária) — é quando os dois concordam que a conclusão tem chão.

### O p-valor preso na borda

O p-valor sai de uma tabela interpolada que vai de 0,01 a 0,99. Fora dela, o
valor é PRESO na borda, sem aviso nenhum: um `0,01` quer dizer "0,01 ou
menos", e um `0,99`, "0,99 ou mais". Quando isso acontece, a `nota` do teste
diz. A decisão a 5% não muda com isso — a ressalva está lá para quem for
reportar o número.

Os termos determinísticos não são parâmetro: o teste do `stats` roda sempre
com constante e tendência.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Teste a hipótese de raiz unitária com correção não paramétrica para dependência serial e heterocedasticidade. Compare o resultado com ADF e KPSS.

## Configuração

Nenhum.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d", "series/diff", from = "log") |>
  tr_add("pp", "series/phillips_perron", from = "d") |>
  tr_add("kpss", "series/kpss", from = "d")
```

## Como interpretar

Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta vários testes num só quadro.

## Veja também

`series/adf`, o teste da mesma hipótese nula por outro caminho; `series/kpss`,
o de hipótese nula oposta; `series/ndiffs` para quantas diferenças a série
pede.

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
