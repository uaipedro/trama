---
title: Friedman
description: "Friedman, Durbin ou Skillings–Mack: os tratamentos diferem dentro dos blocos? (DBC não paramétrico, blocos completos ou incompletos)"
section: colecoes
collection: modelos
node: models/friedman
category: testes
related: [models/anova_dbc, models/kruskal]
---

## O que o bloco faz

`models/friedman` ordena os tratamentos dentro de cada bloco e compara as somas de postos. É a alternativa por postos ao `models/anova_dbc`, com blocos completos ou incompletos. A saída é `models/test`.

O método segue o desenho. Com blocos completos, o teste de Friedman (1937), com o W de Kendall como efeito. Com blocos incompletos balanceados (todo bloco com o mesmo número de tratamentos, todo tratamento o mesmo número de vezes, todo par junto o mesmo número de vezes), o de Durbin (1951). Com faltantes quaisquer, o de Skillings & Mack (1981). Os três respondem à mesma pergunta e por isso ficam no mesmo bloco.

## Quando usar

Num DBC de um fator em que os resíduos não são normais e nenhuma transformação resolve, ou num experimento em blocos incompletos (provadores que avaliam só parte das amostras, parcelas perdidas). Pede no máximo uma observação por bloco e tratamento: com repetições, resuma antes pela média de cada casela.

## Configuração

Informe a resposta numérica, a coluna do tratamento e a do bloco. O **Método** vem em `auto`: o bloco escolhe pelo desenho e a nota diz qual usou. Pode forçar `friedman` (bloco incompleto sai inteiro, e a nota conta quantos), `durbin` (recusa desenho que não é balanceado e aponta o Skillings–Mack) ou `skillings_mack`.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("milho", "models/example", dataset = "milho_dbc") |>
  tr_add("fr", "models/friedman", resposta = "producao", tratamento = "hibrido",
         bloco = "bloco", from = "milho")
```

Os 5 híbridos do `milho_dbc` em 4 blocos dão qui-quadrado de Friedman 13,6 com 4 gl, p = 0,0087, e W de Kendall 0,85: a ordem dos híbridos se repete quase igual em todos os blocos.

Em blocos incompletos, o mesmo bloco muda de método sozinho. Sete provadores que ordenam três de sete variedades de sorvete cada (o exemplo do `agricolae::durbin.test`, t = 7, b = 7, k = 3, r = 3, λ = 1) dão T1 de Durbin 12,0 com 6 gl, p = 0,062. Quatro métodos de montagem em nove blocos com quatro parcelas faltando (o exemplo do pacote `Skillings.Mack`) dão estatística de Skillings–Mack 15,49 com 3 gl, p = 0,0014.

## Como interpretar

A hipótese nula é que, dentro de cada bloco, todos os tratamentos têm a mesma distribuição. Rejeitar diz que algum tratamento difere, não qual. Empates dentro do bloco recebem posto médio; Friedman e Durbin corrigem a estatística, o Skillings–Mack não (fica conservador). O Skillings–Mack supõe que as parcelas se perderam ao acaso, não por causa do tratamento. O p-valor vem da aproximação qui-quadrado, que fica grosseira com poucos blocos (como os 4 do exemplo).
