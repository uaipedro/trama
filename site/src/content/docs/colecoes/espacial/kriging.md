---
title: "Krigagem"
description: "Interpola a variável numa grade recortada na borda, com o erro-padrão de cada célula."
section: colecoes
collection: espacial
node: spatial/kriging
category: "Predizer"
related: [spatial/variogram_fit, spatial/map]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

A krigagem interpola a variável onde ela não foi medida, usando o modelo de
dependência espacial ajustado: cada ponto vizinho pesa conforme a distância e
conforme o que o variograma diz sobre essa distância. É interpolador exato —
numa coordenada amostral devolve o valor observado — e, diferente de qualquer
outro interpolador, **devolve o erro-padrão de cada célula**. O mapa do
erro-padrão é o par honesto do mapa do predito: mostra onde a predição vale
pouco. Use `spatial/map` para ver os dois.

A entrada **pontos** é opcional: o modelo já carrega os pontos com que foi
ajustado, e a krigagem usa esses. Conecte-a só se quiser ver o encadeamento
explícito; se os pontos conectados não forem os mesmos dados do modelo, a
krigagem para com erro em vez de interpolar outro conjunto.
A **krigagem universal** entra pelo param Tipo. Ela não supõe média constante:
estima, junto com a predição, uma tendência de larga escala — um plano (1ª
ordem), uma superfície quadrática (2ª ordem) ou uma covariável conhecida em toda
célula (deriva externa). Use-a quando o variograma só estabiliza depois de
remover tendência: a tendência que você removeu ali e a que escolhe aqui são **a
mesma hipótese**, e as duas devem combinar. Variograma com tendência de 1ª ordem
e krigagem ordinária é incoerente — o modelo descreve o resíduo e a krigagem
prediz o total.

A tendência polinomial é ajustada em coordenada **centrada e padronizada**. Isso
não muda a conta (a krigagem com tendência é invariante a reparametrização
linear da base) e evita o mal condicionamento que coordenada UTM crua produz no
termo quadrático — medimos 3,7e-4 de diferença na 2ª ordem, e o `geoR` chega a
ficar singular.

A **deriva externa** (tendência por covariável) tem uma exigência que não dá
para contornar: a covariável precisa ser conhecida em **toda célula** onde se
prediz, e não só nos pontos amostrais. Por isso ela só roda com uma tabela
ligada na porta **Grade**, trazendo as coordenadas e a covariável. O bloco
recusa quando a covariável falta ou tem célula vazia, em vez de preencher por
conta própria: interpolar a covariável por dentro deixaria o erro-padrão do mapa
subestimado sem avisar, porque o erro dessa interpolação não entra na variância
de krigagem.

## Parâmetros

- **Tipo** — ordinária estima a média a partir dos dados e é o padrão.
  Simples supõe a média da população conhecida e pede que você a informe; é a
  escolha certa só quando a média vem de fora dos dados.
- **Tendência** — só na universal: `1a ordem` (plano), `2a ordem` (superfície
  quadrática) ou `covariavel` (deriva externa, que exige a grade com a
  covariável, pela porta Grade). Tendência constante não aparece aqui porque
  universal com tendência constante é a própria ordinária.
- **Grade** (porta) — uma tabela com as colunas de coordenada, que passa a ser
  a grade de predição. Ligada, ela **vence a Resolução**. É o único caminho da
  deriva externa, e serve também a quem quer predizer em pontos escolhidos em
  vez de numa grade regular. As colunas de coordenada precisam ter os mesmos
  nomes declarados no bloco Coordenadas.
- **Média conhecida** — só na simples. Usar a média amostral aqui não é
  krigagem simples: é fingir que se conhece o que se estimou.
- **Resolução** — pontos no lado maior da grade. A grade cobre os pontos e a
  borda, e é recortada na borda, quando há. O número de células cresce com o
  **quadrado** da resolução e o custo cresce com ele: algumas centenas de
  pontos em resolução 500 levam dezenas de segundos com vizinhança global.
  O remédio é **Vizinhos** (por exemplo 30), que reduz esse tempo várias vezes.
  A nota da superfície diz quantas células saíram.
- **Vizinhos / Raio** — vazios krigam com todos os pontos. Limitar acelera e
  deixa o resultado mais local, mas vizinhança pequena demais produz emenda
  visível entre regiões. O raio está na unidade das coordenadas (metros, nos
  exemplos); célula sem nenhum ponto dentro do raio fica sem predição, e a nota
  diz quantas. Se **nenhuma** célula for predita — raio fora da escala dos
  dados, como 1 em coordenadas UTM — o bloco para com erro em vez de devolver
  um mapa vazio, e a mensagem diz a que distância está o vizinho mais próximo.

## Valor

Uma superfície predita (`spatial/surface`). O adaptador para `data/table` dá uma
linha por célula, com as coordenadas, `predito`, `variancia` e `erro_padrao`.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("p", "spatial/example", dataset = "milho_pr") |>
  tr_add("v", "spatial/variogram", dist_max = 250000, from = "p") |>
  tr_add("m", "spatial/variogram_fit", familia = "esferico", from = "v") |>
  tr_add("k", "spatial/kriging", resolucao = 40L, from = "m")
```

## Veja também

`spatial/variogram_fit` para o modelo que esta krigagem consome; `spatial/map`
para ver o predito e o erro-padrão.

