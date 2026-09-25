---
title: Intervenção
description: "ARIMA com degrau, pulso ou rampa numa data: quanto um evento mudou a série?"
section: colecoes
collection: series-temporais
node: series/intervencao
category: Modelar
order: 2
related: [series/arima, series/pettitt, series/window]
---

## O que o bloco faz

Mede o efeito de um EVENTO numa data conhecida — uma lei, uma mudança de
política, um acidente — sobre a série: o modelo de intervenção de Box e Tiao
(1975). A série é um ARIMA mais um regressor que liga na data:

- **degrau** — 0 antes, 1 da data em diante: o nível MUDOU e ficou.
- **pulso** — 1 só na data: um choque de um período.
- **rampa** — 0 antes, 1, 2, 3, ... a partir da data: a inclinação mudou.

O coeficiente ω do regressor é o efeito, estimado junto com o ARIMA por
máxima verossimilhança; o erro-padrão já leva em conta a autocorrelação, o que
uma comparação ingênua de médias antes e depois não faz.

### A data vem de FORA

A data é informada, não procurada: é o que se sabia antes de olhar o gráfico.
Escolher a data pelo maior salto da própria série e depois testá-la aqui é
testar a hipótese com o dado que a sugeriu, e o p-valor sai otimista. Para
PROCURAR uma quebra, `series/pettitt` ou `series/zivot_andrews`.

### Série em log

Com a série no log (`series/transform`), o degrau é uma mudança
PROPORCIONAL, e a coluna `efeito_pct` = 100·(exp(ω) − 1) a traduz em
porcentagem. Sem log, ignore essa coluna: o efeito é o ω, na unidade da série.

### A ordem do ARIMA

Escolha a ordem do ruído no trecho ANTES da intervenção (`series/window` →
`series/arima` automático) e repita aqui. Com diferenças (d ou D), o regressor
é diferenciado junto: o degrau numa série diferenciada vira um pulso na
diferença, e o ω continua sendo a mudança de nível.

Com **Resposta** = `imediata` (padrão), é a forma de ordem zero: o efeito
entra inteiro na data.

### Resposta gradual

Com **Resposta** = `gradual` (degrau ou pulso), o efeito entra pela função de
transferência de Box e Tiao, ω/(1 − δB): no primeiro período ele vale ω, e
depois cada período soma δ vezes o anterior. No degrau, o efeito cresce (ou
encolhe) até o nível de longo prazo ω/(1 − δ), que sai numa linha própria
(`efeito_longo_prazo`, com erro-padrão pelo método delta); no pulso, o choque
se desfaz aos poucos, à razão δ por período. δ é estimado junto com o ARIMA
por máxima verossimilhança (perfilada em δ), com erro-padrão da hessiana
completa. Conferido contra o `TSA::arimax` (Cryer e Chan, 2008) no tráfego
aéreo dos EUA depois de 11/09/2001: pulso gradual com ω = −0.346 e δ = 0.695,
a menos de 1e-3. Um δ na borda (|δ| > 0.99) é recusado: a resposta não se
estabiliza, e o degrau (ou a rampa) descreve melhor.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes.

## Quando usar

Meça quanto um evento de data conhecida mudou a série, com erro-padrão que
respeita a autocorrelação. No exemplo, a lei do cinto de segurança no Reino
Unido (fevereiro de 1983) sobre o log dos motoristas mortos ou feridos
(`Seatbelts`), com ruído ARIMA(1,0,0)(1,1,1)₁₂. Rodando o exemplo:

```
termo         estimativa   erro-padrão   IC 95%              p           efeito
intervencao     -0,2397       0,0433     [-0,325; -0,155]    3,1e-08     -21,3%
ar1              0,5527       0,0736
sar1             0,1387       0,1168
sma1            -0,8958       0,1147
```

Queda estimada de 21% no nível depois da lei. O ajuste é o mesmo do
`forecast::Arima` com o degrau como `xreg` (conferido a 1e-8), e o degrau é
idêntico à coluna `law` do próprio `Seatbelts`.

## Configuração

- **Data da intervenção** — o período, como `1983, 2` (fevereiro de 1983) ou
  só o ano numa série anual. Precisa haver ao menos uma observação antes.
- **Tipo** — `degrau` (padrão), `pulso` ou `rampa`.
- **p, d, q** e **P, D, Q** — a ordem do ARIMA do ruído.
- **Constante** — média (ou deriva, com uma diferença) no modelo.
- **Resposta** — `imediata` (padrão, ordem zero) ou `gradual` (ω/(1 − δB),
  só degrau e pulso).

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("sb", "series/example", dataset = "Seatbelts$drivers") |>
  tr_add("log", "series/transform", from = "sb") |>
  tr_add("lei", "series/intervencao", data = "1983, 2", p = 1L, d = 0L, q = 0L,
         P = 1L, D = 1L, Q = 1L, from = "log")
```

## Como interpretar

Uma tabela, uma linha por coeficiente, a da intervenção primeiro: `termo`,
`estimativa`, `erro_padrao`, `li_95`, `ls_95` (IC de Wald), `z`, `p_valor` e
`efeito_pct` (só na linha da intervenção). Com resposta gradual, vêm também
a linha `delta` e, no degrau, `efeito_longo_prazo` (com o `efeito_pct` dele);
o `efeito_pct` da linha `intervencao` fica vazio, porque ω é só o primeiro
período.

## Veja também

`series/arima` para escolher a ordem do ruído; `series/pettitt` para
procurar uma data de mudança que não se conhece; `series/window` para
ajustar só o trecho anterior.
