---
title: Do ponto medido ao mapa de krigagem
description: Percorra o fluxo geoestatístico, da tabela com coordenadas ao variograma, ao modelo ajustado e ao mapa com o erro-padrão.
section: colecoes
collection: espacial
order: 1
related: [spatial/read_points, spatial/boundary, spatial/example, spatial/coordinates, spatial/indicator, spatial/explore, spatial/variogram, spatial/anisotropy, spatial/variogram_fit, spatial/kriging, spatial/validation, spatial/map]
---

## Fluxo de trabalho

A coleção `spatial` segue o caminho de uma análise geoestatística: declarar o objeto espacial, olhar os dados, medir a dependência, modelá-la e só então interpolar. O objeto carrega as coordenadas, a projeção, a unidade e, nos exemplos, a borda, e os blocos seguintes não voltam a perguntar onde estão o x e o y.

| Etapa | Blocos | Resultado usado adiante |
| --- | --- | --- |
| Fonte | `spatial/example`, `spatial/read_points` | conjunto pronto, ou a tabela lida de um arquivo vetorial |
| Preparar | `spatial/coordinates`, `spatial/boundary`, `spatial/indicator` | objeto espacial (`spatial/points`), com projeção e borda |
| Explorar | `spatial/explore` | gráfico com mapa, coordenadas e distribuição |
| Variograma | `spatial/variogram`, `spatial/anisotropy`, `spatial/variogram_fit` | variograma empírico, as curvas por direção e o modelo ajustado |
| Predizer | `spatial/kriging`, `spatial/validation`, `spatial/map` | superfície com predito e erro-padrão, a medida do modelo, e o mapa |

### O que entrou na 0.2.0

- **Dado próprio com borda.** `spatial/read_points` lê pontos de shapefile
  zipado, GeoJSON, GeoPackage ou KML; `spatial/boundary` lê o contorno da área;
  e o `spatial/coordinates` ganhou a porta **Borda** mais a opção de fechar a
  borda no **casco convexo dos pontos**, sem precisar de arquivo. Sem borda a
  grade da krigagem é o retângulo da extensão, e o mapa prediz fora da área de
  estudo com cara de resultado válido.
- **Anisotropia.** `spatial/anisotropy` mostra o variograma em várias direções
  de uma vez, com faixa de referência opcional sob isotropia. A leitura é
  visual: o bloco não faz teste de hipótese e não devolve "razão estimada" —
  você lê as curvas e informa a razão e o ângulo no `spatial/variogram_fit`.
- **Validação cruzada.** `spatial/validation` prediz cada ponto sem ele mesmo e
  reporta erro médio, RMSE, **MSDR** e correlação. O MSDR é o que julga o mapa
  de erro-padrão.
- **Krigagem universal e deriva externa.** `spatial/kriging` aceita tendência de
  1ª ordem, 2ª ordem ou por covariável. A deriva externa exige a covariável
  conhecida em toda célula, por uma tabela ligada na porta **Grade**.
- **Krigagem indicadora.** `spatial/indicator` troca a variável por um indicador
  0/1 num corte, e a krigagem passa a estimar **probabilidade**.

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
