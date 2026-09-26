---
title: "Ver o plano"
description: "Mapa, hierarquia, combinações ou ordem de execução de um plano."
section: colecoes
collection: experimentos
node: experiments/view
category: "Planejar"
related: [experiments/design, experiments/effect, experiments/error]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Desenha um plano de `experiments/design` (ou o que sai de `experiments/effect`
e `experiments/error`) por um dos cinco lados:

- **mapa** — a grade 2D (linhas × colunas) com o tratamento de cada unidade e
  o contorno dos blocos. No fracionado e no composto central a grade é a
  sequência das corridas; nas medidas repetidas e no crossover, indivíduo ×
  tempo (período).
- **hierarquia** — os níveis de unidade, do mais alto ao mais fino, com cada
  fator ao lado do nível em que é aplicado e o mecanismo e o escopo do sorteio.
  Fator sem sorteio (bloco, tempo, covariável) sai em cinza.
- **combinacoes** — a grade completa dos níveis dos tratamentos com o número de
  réplicas de cada combinação; célula sem réplica aparece como "vazia" (no
  fracionado, é a fração que ficou de fora).
- **ordem** — o tratamento de cada unidade na ordem de execução.
- **componentes** — a resposta simulada decomposta: uma barra empilhada com a
  contribuição de cada termo (`.ef_*`), e o resíduo quando o erro é normal. O
  intercepto sai da pilha e vai ao subtítulo; o ponto é a resposta menos o
  intercepto. Por **tratamento**, cada barra é a média de cada termo na
  combinação de tratamentos; por **unidade**, uma barra por unidade, na ordem
  de execução. Fora da normal, os termos estão na escala do preditor linear.
  Pede um plano com termos.

## Parâmetros

- **Aba** — `mapa`, `hierarquia`, `combinacoes`, `ordem` ou `componentes`.
- **Componentes por** — `tratamento` ou `unidade` (só na aba `componentes`).

## Valor

Um gráfico (`view/plot`).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "fracionado", fatores = "A; B; C; D",
         geradores = "D = ABC") |>
  tr_add("comb", "experiments/view", aba = "combinacoes", from = "plano")
```

## Veja também

`experiments/design`, `experiments/effect`, `experiments/error`.

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

