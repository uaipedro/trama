---
title: Ljung-Box
description: "Ljung-Box: a série é ruído branco, ou sobrou autocorrelação?"
section: colecoes
collection: series-temporais
node: series/ljung_box
category: Autocorrelação
order: 2
related: [series/residuals, series/box_pierce, series/adf]
---

## O que o bloco faz

Testa se as primeiras autocorrelações da série são, EM CONJUNTO, zero — isto
é, se a série é ruído branco até aquela defasagem. H0 é "as autocorrelações
até a defasagem usada são nulas": p-valor pequeno quer dizer que HÁ
autocorrelação. Não rejeitar não prova ruído branco: a conclusão sai "não há
evidência de autocorrelação", e dependência além da última defasagem o teste
não olha.

O uso típico é nos resíduos de um modelo (`series/residuals`): se sobrou
autocorrelação, o modelo deixou estrutura para trás e ainda há o que ajustar.

### Este ou o Box-Pierce?

Este. Os dois testam a mesma hipótese nula, com a mesma estatística; o
Ljung-Box é a versão CORRIGIDA para amostra pequena, e é a que se reporta. O
`series/box_pierce` é a fórmula original de 1970, e está na coleção para
comparar com trabalho antigo que a usou — não para escolher no cara ou coroa.

### Os graus descontados

Nos resíduos de um modelo, `graus` é o número de parâmetros estimados —
p + q + P + Q de um ARIMA (o `print` no card do modelo mostra os coeficientes;
conte-os). Cada parâmetro ajustado consome um grau de liberdade, e sem
descontá-los o teste fica generoso demais: aceita ruído branco onde há
estrutura. Numa série qualquer, que não é resíduo de nada, deixe 0.

**Defasagens**: quantas autocorrelações entram. `0` é a regra de Hyndman — 10
para série não sazonal, duas vezes o ciclo para sazonal, e nunca mais que um
quinto da série. As defasagens usadas e os graus descontados saem na `nota`.

### Faltantes

Este bloco ACEITA série com faltante, ao contrário dos três blocos de raiz
unitária (`series/adf`, `series/kpss`, `series/phillips_perron`) e do
`series/ndiffs`, que os recusam. Aqui eles são ignorados na conta: o tamanho
usado é o de observações VÁLIDAS, e o bloco pede pelo menos doze delas — com
menos, o nó sai em vermelho, em vez de um veredito calculado sobre uma
defasagem só. É deliberado: o uso típico é diagnosticar resíduo, e resíduo vem
com buraco sempre que o modelo perdeu observação — exigir série cheia recusaria
justamente o caso mais comum.

## Quando usar

Teste se as autocorrelações até uma defasagem escolhida são conjuntamente compatíveis com ruído branco. É um diagnóstico comum dos resíduos após ajustar um modelo.

## Configuração

- **Defasagens** — 0 para automático.
- **Graus do modelo** — parâmetros ajustados, a descontar. Tem de ser menor
  que as defasagens.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("arima", "series/arima", automatico = FALSE, p = 0L, d = 1L, q = 1L,
         P = 0L, D = 1L, Q = 1L, from = "pax") |>
  tr_add("res", "series/residuals", from = "arima") |>
  tr_add("lb", "series/ljung_box", graus = 2L, from = "res")
```

## Como interpretar

Um teste (`data/test`). Ligado numa entrada de tabela, ele vira UMA linha de
relatório: um `data/bind_rows` junta vários testes num só quadro.

## Veja também

`series/residuals` para testar um modelo; `series/acf` para ver em que
defasagem está a autocorrelação; `series/box_pierce`, o mesmo teste sem a
correção de amostra pequena.

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
