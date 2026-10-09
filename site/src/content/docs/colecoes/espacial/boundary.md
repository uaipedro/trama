---
title: "Ler borda de arquivo"
description: "Lê um arquivo vetorial de polígonos como a borda da área de estudo, dissolvida e reprojetada."
section: colecoes
collection: espacial
node: spatial/boundary
category: "Preparar"
related: [spatial/read_points, spatial/coordinates]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Lê um arquivo vetorial de **polígonos** e devolve a borda da área de estudo, que
entra na porta **Borda** do `spatial/coordinates`. A borda faz três coisas
adiante: recorta a grade da krigagem, desenha o contorno no gráfico exploratório
e dispara a guarda que pega borda em escala, projeção ou lugar errado.

Feições múltiplas são **dissolvidas**, e fica o contorno de maior área — uma
malha estadual costuma vir como vários polígonos, por causa das ilhas. Anel
interno (buraco, enclave) é descartado, e a nota do card diz quantos: o tipo
guarda um contorno só.

A borda **é reprojetada** para a projeção dos pontos quando as duas diferem, e a
nota diz de qual para qual. Borda em grau com pontos em metro é reprojetada, não
recusada: a malha do IBGE vem em grau, e a recusa de grau vale para os pontos,
cuja distância o variograma mede, não para o recorte.

## Parâmetros

- **Arquivo** — caminho do `.zip`, `.shp`, `.geojson`, `.gpkg` ou `.kml`.
- **Camada** — só é preciso quando o arquivo tem mais de uma.
- **Reprojetar para (EPSG)** — vazio lê como está; a projeção dos pontos ainda
  será aplicada quando a borda for ligada a eles.

## Valor

Uma borda (`spatial/boundary`), com a área, o número de vértices e a fonte.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("borda", "spatial/boundary", caminho = "pr_uf.zip") |>
  tr_add("ler", "spatial/read_points", caminho = "sedes.zip") |>
  tr_add("pontos", "spatial/coordinates", x = "x", y = "y",
         variavel = "milho_kg_ha", crs = "31982", from = c("ler", "borda"))
```

## Veja também

`spatial/read_points` para os pontos; o param **Borda** do
`spatial/coordinates` para o casco convexo, que não precisa de arquivo.

