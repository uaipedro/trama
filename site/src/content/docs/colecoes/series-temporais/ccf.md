---
title: "Correlação cruzada (CCF)"
description: "A correlação entre duas séries em cada defasagem, negativa e positiva, com a banda do ruído branco."
section: colecoes
collection: series-temporais
node: series/ccf
category: "Ver"
related: [series/acf, series/diff, series/arima, series/residuals, series/join, series/var_select]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

A correlação cruzada: para cada defasagem k, a correlação entre x no tempo
t + k e y no tempo t. Responde a uma pergunta de defasagem: se uma série mexe
antes da outra, em quantos períodos isso aparece?

### Como ler

- barra em **k > 0**: y no tempo t se relaciona com x no tempo t + k, ou seja,
  **y antecede x** por k períodos.
- barra em **k < 0**: o espelho — **x antecede y** por |k| períodos.
- barra em **k = 0**: as duas se movem juntas no mesmo período.
- a linha tracejada é a banda de ±1,96/√n, a do ruído branco. Barra FORA dela
  é candidata a relação, não conclusão: com 40 defasagens, duas passam por
  acaso.

### Cuidado: tendência e autocorrelação enganam

Duas séries que só sobem com o tempo (o DAX e o CAC, por exemplo) têm
correlação cruzada alta em muitas defasagens, sem relação nenhuma entre elas:
a tendência de uma "explica" a da outra. O mesmo vale para qualquer série
muito autocorrelacionada, que tem memória e por isso parece acompanhar a
outra. A banda ±1,96/√n também supõe ruído branco; com autocorrelação ela fica
estreita demais.

As duas séries não aceitam faltantes: o período comum é recortado, e com buraco
dentro dele o `series/interpolate` vem antes.

Antes de ler a defasagem, **diferencie** cada série (`series/diff`) ou
**pré-branqueie**: ajuste um ARIMA em cada uma (`series/arima`) e leia a
correlação cruzada dos resíduos (`series/residuals`). Só então o pico numa
defasagem diz alguma coisa sobre quem antecede quem.

## Parâmetros

- **Defasagens** — até onde ir, dos dois lados. 0 é automático: 10·log10(n),
  mas nunca menos de três ciclos numa série sazonal.

## Valor

Um gráfico (`view/plot`) com uma barra por defasagem, das negativas às
positivas. As barras FORA da banda saem coloridas.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("dax", "series/example", dataset = "EuStockMarkets$DAX") |>
  tr_add("cac", "series/example", dataset = "EuStockMarkets$CAC") |>
  tr_add("dif_dax", "series/diff", from = "dax") |>
  tr_add("dif_cac", "series/diff", from = "cac") |>
  tr_add("cc", "series/ccf", from = c(x = "dif_dax", y = "dif_cac"))
```

## Veja também

`series/acf`, a mesma leitura para uma série só; `series/diff` para tirar a
tendência antes; `series/arima` e `series/residuals` para o pré-branqueamento;
`series/join` para montar a série múltipla que o `series/var_select` lê.

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

