---
title: Do ponto medido ao mapa de krigagem
description: Percorra o fluxo geoestatístico, da tabela com coordenadas ao variograma, ao modelo ajustado e ao mapa com o erro-padrão.
section: colecoes
collection: espacial
order: 1
related: [spatial/example, spatial/coordinates, spatial/explore, spatial/variogram, spatial/variogram_fit, spatial/kriging, spatial/map]
---

## Fluxo de trabalho

A coleção `spatial` segue o caminho de uma análise geoestatística: declarar o objeto espacial, olhar os dados, medir a dependência, modelá-la e só então interpolar. O objeto carrega as coordenadas, a projeção, a unidade e, nos exemplos, a borda, e os blocos seguintes não voltam a perguntar onde estão o x e o y.

| Etapa | Blocos | Resultado usado adiante |
| --- | --- | --- |
| Fonte | `spatial/example`, `spatial/coordinates` | objeto espacial (`spatial/points`) |
| Explorar | `spatial/explore` | gráfico com mapa, coordenadas e distribuição |
| Variograma | `spatial/variogram`, `spatial/variogram_fit` | variograma empírico e modelo ajustado |
| Predizer | `spatial/kriging`, `spatial/map` | superfície com predito e erro-padrão, e o mapa dela |

## Exemplo completo

Sergipe tem 68 municípios, o suficiente para o exemplo rodar rápido. O variograma usa a distância máxima padrão, o modelo é ajustado com os padrões do bloco, e o mapa mostra o erro-padrão, o par honesto do mapa do predito.

```r
library(trama)

reg <- tr_registry()
for (p in c("trama.data", "trama.view", "trama.spatial")) tr_use(p, registry = reg)

tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_se") |>
  tr_add("v", "spatial/variogram", from = "p") |>
  tr_add("m", "spatial/variogram_fit", from = "v") |>
  tr_add("k", "spatial/kriging", from = c("p", "m")) |>
  tr_add("mapa", "spatial/map", mostrar = "erro-padrao", from = "k")
```

Coordenadas em graus são recusadas: o variograma mede distância em linha reta, e um grau de longitude não é uma distância fixa. Projete antes, num CRS em metros. O [catálogo de Geoestatística](/trama/colecoes/espacial/) reúne as configurações de cada bloco.
