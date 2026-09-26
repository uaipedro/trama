---
title: "Superfície de resposta"
description: "Modelo de 1ª ou 2ª ordem em fatores codificados, análise canônica, falta de ajuste e contorno."
section: colecoes
collection: experimentos
node: experiments/response_surface
category: "Analisar"
related: [models/residuals, models/predict, experiments/boxcox]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Ajusta a superfície de resposta aos fatores **codificados** (−1, 0, +1, ±α) —
de 1ª ordem (plano: y = b0 + Σ bᵢxᵢ) ou de 2ª ordem (quadrática completa:
mais os produtos xᵢxⱼ e os quadrados xᵢ²) — e faz a leitura dela:

- **Quadro** — os SQ sequenciais agrupados na ordem dos livros: bloco, primeira
  ordem, interações, quadráticos, resíduo; e, havendo pontos repetidos (os
  centrais), o resíduo partido em **falta de ajuste** e **erro puro**, com o F
  da falta de ajuste. Falta de ajuste significativa diz que o modelo daquela
  ordem não basta.
- **Análise canônica** (2ª ordem) — escrevendo ŷ = b0 + x'b + x'Bx, o ponto
  estacionário é xₛ = −B⁻¹b/2 e ŷₛ = b0 + xₛ'b/2. Os autovalores de B dão a
  natureza: todos negativos, máximo; todos positivos, mínimo; sinais mistos,
  ponto de sela. Autovalor perto de zero (menos de 5% do maior em módulo) é
  sinalizado como cumeeira — o critério de 5% é uma convenção deste bloco. Se
  o ponto cai fora da região experimentada, a nota avisa: é extrapolação.
- **1ª ordem** — no lugar da canônica, a direção de maior subida (o vetor b
  normalizado), o passo do método de Box & Wilson.
- **Contorno** — ŷ nos dois primeiros fatores, com os demais no centro (0), os
  pontos do delineamento e o ponto estacionário marcado.

O modelo sai como `models/fit` comum (o `lm` do `models/lm`): resíduos,
pressupostos, coeficientes e previsão funcionam nele. Com bloco, o bloco entra
aditivo, e o b0 da canônica e do contorno é a média dos blocos.

## Parâmetros

- **Resposta** — coluna numérica.
- **Fatores** — de 1 a 6 colunas numéricas codificadas, separadas por vírgula.
- **Ordem** — `1` (plano) ou `2` (quadrática completa).
- **Bloco** — coluna do bloco (opcional).

## Valor

Quatro portas: `modelo` (`models/fit`); `quadro` (`models/effects`, a ANOVA da
superfície com falta de ajuste e erro puro); `canonica` (`data/table`: ponto
estacionário, ŷ nele, autovalores com os autovetores na nota, natureza e
distância ao centro; na 1ª ordem, a direção de maior subida); `grafico`
(`view/plot`, o contorno).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "composto_central", fatores = "x1; x2") |>
  tr_add("ccd", "data/mutate", name = "rendimento",
         expr = "80 - (x1 - 0.4)^2 - 2 * x2^2 + sin(unidade) / 5", from = "plano") |>
  tr_add("rsm", "experiments/response_surface", resposta = "rendimento",
         fatores = "x1, x2", ordem = "2", from = "ccd")
```

## Veja também

`models/residuals` e `models/predict` sobre o `modelo`; `experiments/boxcox`.

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

