---
title: Autocorrelação parcial (PACF)
description: "A correlação com a defasagem k, descontadas as defasagens intermediárias."
section: colecoes
collection: series-temporais
node: series/pacf
category: Ver
order: 2
related: [series/acf, series/arima]
---

## O que o bloco faz

A autocorrelação PARCIAL: a correlação da série com ela mesma k períodos
antes, depois de descontar o que as defasagens 1 a k−1 já explicam. Se hoje
depende só de ontem, a ACF da defasagem 2 é alta (ontem dependia de anteontem),
mas a PACF da 2 é zero.

Lê-se junto com o `series/acf`:

| | ACF | PACF |
|---|---|---|
| AR(p) | decai aos poucos | corta depois de p |
| MA(q) | corta depois de q | decai aos poucos |
| ARMA | decai | decai |

Picos nos múltiplos do ciclo sugerem termos sazonais (P no ARIMA).

Banda, cores e eixo como no correlograma.

## Quando usar

Examine a associação de cada defasagem depois de controlar as defasagens intermediárias. A PACF ajuda a reconhecer ordens autorregressivas candidatas.

## Configuração

- **Defasagens** — até onde ir. 0 é automático, como no `series/acf`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.series", registry = reg)

tr_flow(reg) |>
  tr_add("lynx", "series/example", dataset = "lynx") |>
  tr_add("log", "series/transform", from = "lynx") |>
  tr_add("pacf", "series/pacf", from = "log")
```

## Como interpretar

Um gráfico (`view/plot`).

## Veja também

`series/acf`, que se lê junto; `series/arima` para ajustar a ordem sugerida.

### Aparência (comum a todos os gráficos)

- **Proporção** — a forma da imagem: `16:9` e `2:1` para paisagem, `1:1` para
  quadrado, `4:3` e `3:4` para o que vai numa página. A imagem sai sempre com
  1600 px no lado maior; o card só a escala, e um clique a abre em tela cheia.
  Mudar a proporção RECOMPUTA o gráfico — é o único param de aparência que
  muda mesmo o desenho, porque o ggplot recoloca a legenda e remede os rótulos.
  Arrastar a alça do card, não: aquilo é tamanho, não proporção.
- **Tema** — `padrão` segue o tema padrão do projeto; os temas (fundo, cores,
  fonte, paleta) são do projeto, e não do gráfico. Sem temas próprios valem os
  embutidos, com `escuro` como padrão; `claro` e `clássico` servem bem ao que
  sai no relatório. O tema dos gráficos não segue o claro/escuro do editor, que
  é preferência de cada pessoa: para mudar todos de uma vez, troque o tema
  padrão em ⚙ Configurações. A paleta só entra onde o
  gráfico não escolheu cores por conta própria.
- **Título**, **Rótulo do X**, **Rótulo do Y** — em branco, o gráfico usa o
  nome da coluna, que costuma ser a legenda certa.
- **Legenda** — `nenhuma` quando a cor já está explicada no título.
