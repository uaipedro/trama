---
title: "Anisotropia"
description: "Variograma em várias direções de uma vez, para ver se a dependência espacial tem alcance diferente conforme a direção."
section: colecoes
collection: espacial
node: spatial/anisotropy
category: "Variograma"
related: [spatial/variogram, spatial/variogram_fit]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Anisotropia é a dependência espacial ter **alcance diferente conforme a
direção**: a variável se parece consigo mesma por mais longe num rumo que no
outro. O variograma omnidirecional esconde isso, porque mistura todas as
direções numa curva só. Este bloco separa.

A leitura é **visual**: curvas que sobem junto e estabilizam no mesmo lugar
indicam isotropia; uma curva que estabiliza muito mais longe que as outras
indica o eixo de maior continuidade. O bloco **não faz teste de hipótese**, e
não devolve "razão de anisotropia".

Isso é decisão medida, não omissão. Três maneiras de estimar razão e ângulo
automaticamente foram testadas, e as três reprovaram; a menos ruim devolve
razão perto de 3 para campos **isotrópicos**, indistinguível do que devolve
para campos de razão 3 de verdade. Um número que não separa o caso do seu
contrário não ajuda ninguém. Olhe as curvas, decida, e digite a razão e o
ângulo no bloco `spatial/variogram_fit`.

Cada direção recebe só uma fração dos pares, então classes de menos ou
tolerância estreita deixam direções sem pares bastantes; a nota do card diz
quais ficaram de fora.

A **faixa de referência** ajuda a calibrar o olho: ela mostra onde as curvas
cairiam se a dependência fosse isotrópica, simulando campos isotrópicos nestes
mesmos pontos. A hipótese que ela representa é "isotrópico com esta estrutura",
e não "sem dependência nenhuma". **Não é teste**: medimos que a fração do
observado dentro da faixa dá cerca de 0,95 sob isotropia e 0,84 sob anisotropia
de razão 3, valores que se sobrepõem. O que separa é o padrão — curvas do eixo
maior e do menor escapando de forma sistemática —, não a contagem.

## Parâmetros

- **Direções** — graus separados por vírgula, de 0 a 180, no sentido horário a
  partir do Norte (a convenção da bússola). O variograma não distingue uma
  direção da oposta, então 0 e 180 seriam a mesma curva.
- **Estimador** — `classico` ou `robusto`, como no `spatial/variogram`.
- **Distância máxima**, **Classes**, **Mínimo de pares** — o mesmo do
  `spatial/variogram`, mas lembre que aqui os pares se dividem entre as
  direções.
- **Tolerância angular** — meia-abertura da janela de cada direção, em graus. A
  90 graus a janela cobre tudo e cada curva vira o omnidirecional.
- **Tendência** — removida antes, como no `spatial/variogram`.
- **Faixa de referência** — desenha, por direção, a faixa que curvas
  **isotrópicas** ocupariam nestes mesmos pontos. Curva que escapa da própria
  faixa, de forma sistemática, é o sinal de anisotropia.
- **Simulações** — quantos campos isotrópicos simular (padrão 19). Mais
  simulações dão faixa mais estável e card mais lento.
- **Semente** — fixa a simulação, para o card não mudar a cada execução.

## Valor

Um objeto de anisotropia (`spatial/anisotropy`), com uma curva por direção.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("pontos", "spatial/example", dataset = "milho_pr") |>
  tr_add("aniso", "spatial/anisotropy", direcoes = "0,45,90,135", from = "pontos")
```

## Veja também

`spatial/variogram` para uma direção só, ou nenhuma;
`spatial/variogram_fit`, que é onde a razão e o ângulo são informados.

