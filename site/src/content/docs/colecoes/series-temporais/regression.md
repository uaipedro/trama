---
title: Regressão dos componentes
description: "Estima tendência e sazonalidade por regressão, com coeficientes e p-valores."
section: colecoes
collection: series-temporais
node: series/regression
category: Regressão
order: 2
related: [series/decompose, series/stl, series/component]
---

## O que o bloco faz

Ajusta um modelo EXPLÍCITO para os componentes da série:

`valor = tendência(t) + sazonal(estação) + erro`

A tendência é um polinômio no tempo (`t`, `t²`, `t³`), e a sazonalidade, uma
variável indicadora por período — onze dummies numa série mensal. É a
decomposição que se faz com regressão, e a diferença para `series/decompose` e
`series/stl` é que aqui cada componente tem COEFICIENTE, erro-padrão e
p-valor: dá para testar se ele existe, em vez de olhar o gráfico e achar.

### O que sai

O card mostra o ajuste como um estatístico o lê: coeficientes, significância,
R² e o F global. Ligado num nó da `data`, vira a tabela de coeficientes
(`termo`, `estimativa`, `erro_padrao`, `estatistica_t`, `p_valor`). Ligado em
`series/component` ou `series/plot_decomposition`, vira decomposição — os
componentes estimados, que somam a série de volta.

### Contraste

Muda como os coeficientes são lidos, e **não** a decomposição:

- **soma_zero** — cada coeficiente sazonal é o desvio daquele período em
  relação à média do ano, e os desvios somam zero. É a convenção da
  decomposição clássica, e o que torna o sazonal daqui comparável ao de lá.
- **categoria_base** — cada coeficiente é a diferença para o primeiro período
  (janeiro, numa mensal). É o `summary()` do R, e o que se vê no livro-texto.

Em ambos, o componente sazonal devolvido é centrado em zero e a tendência
absorve a média — senão a mesma série daria duas decomposições diferentes por
causa de uma escolha de leitura.

### Erro autocorrelacionado

O padrão (**Erro = independente**) é mínimos quadrados ordinários, que supõe
erros independentes. Em série temporal o erro quase sempre é autocorrelacionado,
e aí os erros-padrão saem pequenos demais e os p-valores — dos coeficientes e dos
três F — OTIMISTAS. Confira: `series/component` (`resto`) → `series/ljung_box`.

**Erro = arma** ajusta a mesma regressão por mínimos quadrados generalizados com
erro ARMA(p, q) (`nlme::gls` com `corARMA`, por máxima verossimilhança), com
**Ordem AR do erro** = p e **Ordem MA do erro** = q; AR(1) é o ponto de partida
usual. Os coeficientes passam a ser os do GLS, e os três F viram testes de Wald
com a covariância do GLS (a `nota` do teste diz). Medido sem tendência nenhuma e
com erro AR(1) de phi = 0.6 (n = 120, 300 réplicas), o F de tendência rejeita a
5% em 37% das vezes por MQO e em 8% pelo GLS; com phi = 0.9 são 69% e 17% — perto
da raiz unitária o GLS melhora muito, mas ainda passa do nominal, e o caminho é
diferenciar a série; quando o AR estimado tem raiz inversa de 0.9 ou mais, o
bloco avisa e a `nota` dos F diz. Série que o modelo reproduz sem resíduo é
recusada (não há erro a modelar). O R² e o F do `summary` do MQO não existem no GLS: o card
mostra o resumo do `gls`.

### Limites

O bloco não aceita faltantes, e sazonalidade pede frequência maior que 1. Grau 0
com sazonalidade desligada não tem o que estimar, e para o nó em vermelho. A
entrada **regressor** está declarada mas ainda não é usada.

As potências do tempo são cruas (`t`, `t²`, `t³`) e fortemente
correlacionadas entre si: em `series/example` com grau 3, a matriz de desenho
tem número de condição de 7,3 milhões, e `t³` sai com p = 0,46 enquanto o F do
bloco de tendência é esmagador. Do **grau 2 em diante, os p-valores individuais
dos termos de tendência não se leem um a um** — quem quer saber se há tendência
lê o F do bloco, em `series/f_tendencia`. Os coeficientes sazonais não
sofrem disso.

O nó não prevê, e é de propósito: tendência polinomial fora da amostra é das
extrapolações mais perigosas que existem — um grau 3 dispara para o infinito
logo depois do último ponto. Para prever, `series/arima` ou `series/ets`.

## Quando usar

Estime tendência e sazonalidade por regressão para obter coeficientes, medidas de ajuste e componentes ajustados. A forma da tendência é uma escolha do modelo.

## Configuração

- **Grau da tendência** — 0 (sem tendência) a 3.
- **Sazonalidade** — inclui as dummies de período.
- **Contraste** — `soma_zero` ou `categoria_base`.
- **Erro** — `independente` (MQO, padrão) ou `arma` (GLS com erro ARMA).
- **Ordem AR do erro** e **Ordem MA do erro** — p e q do erro ARMA, de 0 a 3
  (não os dois zero). Só valem com `arma`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("pax", "series/example") |>
  tr_add("reg", "series/regression", grau = 2L, from = "pax") |>
  tr_add("f", "series/f_global", from = "reg") |>
  tr_add("resto", "series/component", componente = "resto", from = "reg") |>
  tr_add("ruido", "series/ljung_box", from = "resto")
```

## Como interpretar

Uma regressão (`series/regression`): o card traz o resumo do ajuste.
`series/f_global`, `series/f_sazonal` e `series/f_tendencia` testam os blocos;
`series/component` extrai um componente como série; ligada à `data`, vira a
tabela de coeficientes.

## Veja também

`series/f_global`, `series/f_sazonal` e `series/f_tendencia` para a
significância dos blocos; `series/decompose` e `series/stl` para as
decomposições não paramétricas; `series/transform` para ajustar em log quando a
oscilação cresce com o nível.
