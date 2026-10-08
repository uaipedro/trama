---
title: "Declarar coordenadas"
description: "Transforma uma tabela em objeto espacial: diz quais colunas são as coordenadas, a variável, o CRS e a unidade."
section: colecoes
collection: espacial
node: spatial/coordinates
category: "Preparar"
related: [spatial/example]
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

Neste bloco o objeto sai **sem borda**: a borda só vem dos conjuntos de exemplo
desta versão (`spatial/example`). A função R `tr_spatial_coordinates()` aceita
uma borda diretamente, no argumento `borda`.

## Parâmetros

- **Coordenada X**, **Coordenada Y** — colunas numéricas, em CRS projetado.
- **Variável** — a variável regionalizada. Linha sem valor nela sai do cálculo,
  e a nota do objeto diz quantas.
- **Covariáveis** — colunas candidatas a tendência externa (opcional).
- **CRS (EPSG)** — código EPSG da projeção (por exemplo, 31983 para o UTM 23S).
  Vazio, trata como plano arbitrário.
- **Unidade da distância** — só rotula eixos e alcance (por exemplo, `m`).
- **Nome** — título do objeto nos cartões e gráficos adiante.

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

`spatial/example` para um conjunto pronto.

