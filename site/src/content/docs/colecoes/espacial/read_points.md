---
title: "Ler pontos de arquivo"
description: "Lê um arquivo vetorial de pontos (shapefile zipado, GeoJSON, GeoPackage, KML) como tabela, com as coordenadas em duas colunas."
section: colecoes
collection: espacial
node: spatial/read_points
category: "Fonte"
related: [spatial/boundary, data/read]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Lê um arquivo vetorial de **pontos** e devolve uma tabela: os atributos do
arquivo mais duas colunas com as coordenadas. Daqui o caminho segue pelo
`spatial/coordinates`, que é onde se diz qual coluna é a variável.

Um shapefile chega quase sempre como **zip** (`.shp` + `.shx` + `.dbf` +
`.prj`), e o bloco abre o zip direto, sem descompactar, lendo a projeção do
`.prj`. Também lê GeoJSON, GeoPackage e KML. Se o zip tiver o shapefile numa
subpasta, não há nada a fazer: o bloco acha. Se tiver mais de uma camada, ele
pede que você escolha e lista as que encontrou.

**GeoJSON é latitude e longitude por especificação**, e o `spatial/coordinates`
recusa grau, porque um grau de longitude não é uma distância fixa — vale cerca
de 111 km no equador e menos conforme a latitude sobe. Por isso existe
**Reprojetar para**: preencha com o EPSG projetado da sua região (por exemplo
31982, UTM 22S) e o arquivo entra já em metro.

## Parâmetros

- **Arquivo** — caminho do `.zip`, `.shp`, `.geojson`, `.gpkg` ou `.kml`.
- **Camada** — só é preciso quando o arquivo tem mais de uma; o erro lista as
  que há.
- **Reprojetar para (EPSG)** — vazio lê como está.
- **Nomes das coordenadas** — dois nomes separados por vírgula. Se o arquivo já
  tiver coluna com esse nome, o bloco recusa em vez de sobrescrever o atributo.

## Valor

Uma tabela (`data/table`) com os atributos do arquivo e as duas colunas de
coordenada.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("ler", "spatial/read_points", caminho = "sedes.zip", crs_saida = "31982") |>
  tr_add("pontos", "spatial/coordinates", x = "x", y = "y",
         variavel = "milho_kg_ha", crs = "31982", from = "ler")
```

## Veja também

`spatial/boundary` para o contorno da área; `data/read` para tabela sem
geometria.

