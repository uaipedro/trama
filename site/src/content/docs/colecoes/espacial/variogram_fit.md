---
title: "Ajustar modelo"
description: "Ajusta um modelo teórico ao variograma empírico e reporta pepita, contribuição e alcance."
section: colecoes
collection: espacial
node: spatial/variogram_fit
category: "Variograma"
related: [spatial/kriging]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Ajusta uma curva teórica aos pontos do variograma empírico. O que sai daqui é o
que a krigagem usa: sem modelo ajustado não há interpolação.

Três números resumem a estrutura espacial: a **pepita** (variância a distância
zero: erro de medida e variação em escala menor que a malha), a **contribuição**
(quanto a estrutura espacial acrescenta) e o **alcance** (até onde ela vai).

O **alcance prático** é o que se lê no gráfico, e não é o parâmetro do modelo.
No esférico são iguais, porque o modelo atinge o patamar exatamente nesse ponto.
No exponencial o prático é cerca de três vezes o parâmetro; no gaussiano, cerca
de 1,73 vez. O bloco reporta os dois.

O bloco **não escolhe a família por você**: comparar modelos é trabalho de
validação cruzada.

## Parâmetros

- **Família** — esférico atinge o patamar; exponencial e gaussiano só se
  aproximam. O gaussiano supõe um fenômeno muito suave perto da origem e costuma
  precisar de pepita.
- **Método** — WLS-Cressie pesa cada classe por `N/γ²`, dando peso às classes
  curtas, que são as que a krigagem usa; é o padrão da literatura e desta
  coleção. WLS-np pesa só pelo número de pares. OLS não pesa.
  O `sqr` que o bloco reporta é a soma de quadrados ponderada com os pesos do
  método escolhido: só se compara entre ajustes do mesmo método e do mesmo
  variograma empírico.
- **Pepita fixa** — mantém a pepita no valor inicial em vez de estimá-la.
- **Valores iniciais** — o ajuste é sensível ao chute, então o bloco **tenta
  várias partidas**, sempre as mesmas (mesma entrada, mesmo modelo): pepita = γ
  da primeira classe ou zero; contribuição = γ máximo menos a pepita; alcance =
  a distância máxima dividida por 3, 6 e 2. Fica o ajuste válido de menor `sqr`.
  Ajuste que sai singular, não converge, tem sinal errado ou alcance prático fora da
  escala dos dados é descartado; se nenhuma partida serve, o bloco dá erro em vez
  de devolver um modelo ruim. Valor que você informa vale como está e não entra
  na grade. Se o único defeito for pepita negativa, use **Pepita fixa** com
  pepita inicial 0.
- **Kappa** — só para Matérn: 0,5 reproduz o exponencial, valores altos
  aproximam o gaussiano.

## Valor

Um modelo ajustado (`spatial/model`). O adaptador para `data/table` dá uma linha
por parâmetro numérico, pronta para comparar duas famílias com `data/bind_rows`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_pr") |>
  tr_add("v", "spatial/variogram", dist_max = 250000, from = "p") |>
  tr_add("m", "spatial/variogram_fit", familia = "esferico", from = "v")
```

## Veja também

`spatial/kriging` para interpolar com este modelo.

