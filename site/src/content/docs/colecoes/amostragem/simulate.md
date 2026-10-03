---
title: Simular desenho
description: Re-sorteia a amostra centenas de vezes e mede viés, erro padrão e cobertura contra a verdade.
section: colecoes
collection: amostragem
node: sampling/simulate
related: [sampling/plot_simulation, sampling/srs, sampling/stratified, sampling/cluster]
---

## O que o bloco faz

Avalia o DESENHO, e não a amostra: pega a receita guardada na amostra ligada
(o bloco de seleção, o n, os estratos, a pós-estratificação), sorteia de novo da
mesma população **Repetições** vezes, estima em cada uma, e compara com a
verdade — que existe, porque a população inteira está no cadastro.

O card é o histograma das estimativas, com a verdade (linha cheia) e a média
das estimativas (tracejada). Embaixo:

- **EP** — o erro padrão EMPÍRICO, o desvio das estimativas entre amostras. É o
  erro que o desenho de fato tem. Entre parênteses, o erro padrão que o
  estimador CALCULA, em média: se os dois batem, o card de uma amostra só é
  confiável.
- **viés** — quanto a média das estimativas se afasta da verdade, em %. Média
  e total pelo desenho são (quase) sem viés; um estimador que ignora o peso não
  seria.
- **cobre** — em quantas amostras o intervalo de confiança contém a verdade.
  Deve ficar perto da confiança (95%); abaixo, o intervalo promete mais do que
  entrega.

Duas simulações lado a lado, com o mesmo n e desenhos diferentes, são a
comparação de eficiência: o desenho de EP menor precisa de menos amostra para a
mesma precisão. Para desenhá-las juntas, `sampling/plot_simulation`; para uma
tabela, ligue as duas num `data/bind_rows` (o adaptador dá uma linha de resumo
por simulação).

Só funciona com amostra de bloco de seleção: a de `sampling/design` não guarda
a população.

## Quando usar

Use para examinar, sob uma população conhecida, viés, erro padrão e cobertura esperados de uma receita de seleção.

## Configuração

- **Variável** — coluna numérica da população.
- **Estimador** — `média` ou `total`.
- **Repetições** — quantas amostras sortear (20 a 10.000). 500 dão a cobertura
  com erro de ±2 pontos.
- **Confiança** — a do intervalo cuja cobertura se mede.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/srs", n = 200L, from = "pop") |>
  tr_add("simulacao", "sampling/simulate", variavel = "producao_t", repeticoes = 200L,
         from = "amostra")
```

## Como interpretar

O resumo compara estimativa média com a verdade, mede viés e erro padrão empírico, e calcula a cobertura dos intervalos. Cobertura observada perto da confiança configurada indica calibração adequada neste cenário simulado. A saída é Simulação `sampling/simulation` com resultados das repetições.

## Veja também

`sampling/plot_simulation`, `sampling/srs`, `sampling/stratified`, `sampling/cluster`.
