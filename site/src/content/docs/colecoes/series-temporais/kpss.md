---
title: KPSS
description: "KPSS: a série é estacionária? (H0 é a estacionariedade)"
section: colecoes
collection: series-temporais
node: series/kpss
category: Raiz unitária
order: 2
related: [series/adf, series/phillips_perron, series/interpolate]
---

## O que o bloco faz

Testa se a série é ESTACIONÁRIA em torno de um nível (ou de uma reta, com
tendência). É o espelho do `series/adf`, e o único teste da coleção cuja
hipótese nula é a estacionariedade.

### A leitura, e por que ela é o contrário do ADF

| teste | H0 | rejeitar H0 quer dizer |
|---|---|---|
| `series/adf` | raiz unitária | estacionária |
| `series/phillips_perron` | raiz unitária | estacionária |
| KPSS | estacionária | não estacionária |

Rejeitar aqui é concluir NÃO estacionária. A rejeição vem da cauda de cima: a
estatística grande é a que derruba H0.

Com amostra pequena, qualquer um desses testes "não rejeita" por falta de
poder — o ADF por falta de evidência contra a raiz unitária, o KPSS por falta
de evidência contra a estacionariedade. Por isso eles andam em par: é a
CONCORDÂNCIA dos dois (o ADF rejeita, o KPSS não) que dá chão à conclusão. Se
discordam, a série está no limite, e diferenciar é o lado seguro.

### O que sai

A estatística `eta`, a tabela de valores críticos a 10%, 5% e 1% e a conclusão
em palavras. O `urca` só publica a tabela: o `p_valor` sai em branco, e não
interpolado a partir dela. A decisão a 5% vem do valor crítico.

**Termos determinísticos**: `constante` para série que oscila em torno de um
nível; `tendência` para série que pode ser estacionária em torno de uma reta —
a tabela de críticos é outra em cada caso.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes, ou recorte a parte cheia com `series/window`.

## Quando usar

Avalie a hipótese de estacionariedade da série. Compare com o ADF, cuja hipótese nula é diferente, e considere se a tendência determinística está incluída.

## Configuração

- **Termos determinísticos** — `constante` ou `tendência`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("log", "series/transform", from = "pax") |>
  tr_add("d", "series/diff", from = "log") |>
  tr_add("kpss", "series/kpss", from = "d") |>
  tr_add("adf", "series/adf", from = "d")
```

## Como interpretar

Um teste (`series/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta o KPSS e o ADF no mesmo quadro.

## Veja também

`series/adf` e `series/phillips_perron`, os testes de hipótese nula oposta —
é com um deles ao lado que este se lê; `series/ndiffs` para quantas diferenças
a série pede; `series/diff` para fazê-las.

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
