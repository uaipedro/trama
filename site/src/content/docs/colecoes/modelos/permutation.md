---
title: Permutação
description: "Teste de permutação de um termo do ajuste linear: compara o F observado com os F obtidos permutando a resposta."
section: colecoes
collection: modelos
node: models/permutation
category: testes
related: [models/anova_table, models/bootstrap]
---

## O que o bloco faz

`models/permutation` permuta os valores da resposta entre as linhas do ajuste, mantendo os preditores, e recalcula o F do **Termo** escolhido em cada repetição. O F observado é comparado com essa distribuição. O cálculo usa a mesma decomposição QR do ajuste, sem reajustar, porque a matriz do modelo não muda.

O F é o sequencial (tipo I). O p-valor de Monte Carlo é `(excedências + 1) / (reamostras + 1)`, com empates contados por uma tolerância de `1e-8`. A tabela mostra também o p-valor teórico do F, para comparação.

## Quando usar

Quando a distribuição F teórica é duvidosa, por exemplo com erros claramente não normais e poucas repetições, e o ajuste é linear (`lm`), o que inclui as ANOVAs de efeitos fixos. Se o experimento tem plano de casualização conhecido, prefira o teste de randomização da coleção de experimentos: este bloco não substitui a casualização do delineamento.

## Configuração

- **Termo** — o rótulo de uma linha do quadro da ANOVA; em branco, usa o primeiro.
- **Dentro de** — coluna que define grupos de permutação. Em branco, permuta entre todas as linhas.
- **Reamostras** — o número de permutações (padrão 9999).

Quando o termo vem depois de outro que tem efeito, como no DBC (`bloco + tratamento`), as linhas só são permutáveis sob H0 dentro do mesmo bloco. Nesse caso, use **Dentro de** = bloco; sem isso, o teste deixa de ser exato.

## Exemplo

```r
library(trama)

reg <- tr_registry()
tr_use("trama.models", registry = reg)

tr_flow(reg) |>
  tr_add("dados", "models/example", dataset = "PlantGrowth") |>
  tr_add("ajuste", "models/anova_dic", resposta = "weight", tratamento = "group", from = "dados") |>
  tr_add("perm", "models/permutation", termo = "group", reamostras = 9999, from = "ajuste")
```

## Como interpretar

A hipótese nula é que o termo não tem efeito, de modo que qualquer rearranjo da resposta seria igualmente provável. O p-valor de permutação diz em que fração das permutações o F foi pelo menos tão grande quanto o observado. Se ele ficar muito diferente do p-valor teórico, os pressupostos do F merecem atenção. O resultado depende do sorteio; a semente é a do card, e o mesmo card dá a mesma distribuição.
