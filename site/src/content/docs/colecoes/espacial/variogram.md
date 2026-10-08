---
title: "Variograma"
description: "Variograma empírico, clássico ou robusto, omnidirecional ou numa direção."
section: colecoes
collection: espacial
node: spatial/variogram
category: "Variograma"
related: [spatial/variogram_fit, spatial/explore]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

A semivariância média entre pares de pontos, por classe de distância. É a
medida de dependência espacial: se o variograma sobe com a distância e
estabiliza, há estrutura espacial, e o alcance diz até onde ela vai.

Um variograma plano desde a primeira classe não quer dizer "sem estrutura":
pode ser escala errada, poucos pares, ou tendência de larga escala não removida.
As distâncias estão na unidade das coordenadas (metros, nos exemplos).

## Parâmetros

- **Estimador** — clássico é o de Matheron (1963), a média das diferenças ao
  quadrado. Robusto é o de Cressie-Hawkins (1980), que resiste a discrepante:
  use quando a nuvem do variograma mostrar valores altos isolados.
- **Classes** — em quantas faixas de distância, de larguras iguais, os pares são
  agrupados.
- **Distância máxima** — vazio usa a diagonal da área dividida por três. Além da
  metade da maior distância, sobram pares de menos para a média significar algo.
- **Tendência removida** — o variograma é calculado nos resíduos. Tendência de
  larga escala não removida faz o variograma subir sem parar e imitar ausência
  de patamar.
- **Direção** — em graus no sentido horário a partir do **Norte** (0 = Norte,
  90 = Leste), a convenção da bússola. Vazio calcula omnidirecional. Comparar
  duas direções é como se enxerga anisotropia.
- **Tolerância angular** — meia-abertura do setor, em graus.
- **Mínimo de pares** — classe com menos pares que isto sai, e a nota diz
  quantas saíram.

## Valor

Um variograma empírico (`spatial/variogram`). O adaptador para `data/table` dá
uma linha por classe, com `u` (distância média), `gamma` (semivariância) e `np`
(pares).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_pr") |>
  tr_add("v", "spatial/variogram", dist_max = 250000, n_classes = 12, from = "p")
```

## Veja também

`spatial/variogram_fit` para ajustar o modelo teórico; `spatial/explore` para
ver a tendência antes.

