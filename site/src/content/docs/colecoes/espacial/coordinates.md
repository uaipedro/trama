---
title: "Declarar coordenadas"
description: "Transforma uma tabela em objeto espacial: diz quais colunas são as coordenadas, a variável, o CRS e a unidade."
section: colecoes
collection: espacial
node: spatial/coordinates
category: "Preparar"
related: [spatial/example, spatial/read_points, spatial/boundary]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

É aqui que a tabela vira objeto espacial. O que sai daqui carrega as
coordenadas, a projeção, a unidade e a borda, e nenhum bloco adiante volta a
perguntar onde está o x.

As coordenadas precisam estar **projetadas**. Latitude e longitude em graus
são recusadas: o variograma mede distância em linha reta, e um grau de
longitude não é uma distância fixa — vale cerca de 111 km no equador e menos
conforme a latitude sobe.

A **borda** da área de estudo entra por aqui, e vale a pena: ela recorta a grade
da krigagem, desenha o contorno no gráfico exploratório e dispara a guarda que
pega borda em escala, projeção ou lugar errado. Sem borda, a grade é o retângulo
da extensão dos pontos, e o mapa prediz fora da área de estudo com cara de
resultado válido.

Dois caminhos: ligar um bloco `spatial/boundary` na porta **Borda**, que lê o
contorno de um arquivo vetorial; ou, sem arquivo nenhum, pôr o param **Borda** em
*casco convexo dos pontos*, que usa o menor polígono convexo que contém a amostra.
Borda ligada na porta vence o param.

Quando as projeções diferem, a borda **é reprojetada** para a dos pontos, e a nota
do objeto diz de qual para qual. Borda em grau com pontos em metro é reprojetada,
não recusada: a malha do IBGE vem em grau, e a recusa de grau vale para os
pontos, cuja distância o variograma mede, não para o recorte. O que o bloco
recusa é borda **sem** projeção declarada quando os pontos têm uma: aí não há
como saber em que plano a borda está.

## Parâmetros

- **Coordenada X**, **Coordenada Y** — colunas numéricas, em CRS projetado.
- **Variável** — a variável regionalizada. Linha sem valor nela sai do cálculo,
  e a nota do objeto diz quantas.
- **Covariáveis** — colunas candidatas a tendência externa (opcional).
- **CRS (EPSG)** — código EPSG da projeção (por exemplo, 31983 para o UTM 23S).
  Vazio, trata como plano arbitrário.
- **Unidade da distância** — só rotula eixos e alcance (por exemplo, `m`).
- **Nome** — título do objeto nos cartões e gráficos adiante.
- **Borda** — o que fazer quando nenhuma borda é ligada na porta: `nenhuma`, ou
  `casco convexo dos pontos`. Pontos colineares não formam casco, e o bloco diz.

## Valor

Um objeto espacial (`spatial/points`).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("tab", "data/example") |>
  tr_add("pontos", "spatial/coordinates", x = "leste", y = "norte",
         variavel = "milho_kg_ha", crs = "31982", unidade = "m", from = "tab")
```

## Veja também

`spatial/example` para um conjunto pronto; `spatial/read_points` para ler os
pontos de um arquivo vetorial; `spatial/boundary` para a borda.

