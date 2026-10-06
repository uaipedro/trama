---
title: Detectar intervenções
description: "Procura datas candidatas a intervenção (pulso, degrau, temporária, inovacional) pelo método de Chen e Liu."
section: colecoes
collection: series-temporais
node: series/detect_interventions
category: Modelar
order: 3
related: [series/intervencao, series/pettitt, series/zivot_andrews, series/residuals]
---

## O que o bloco faz

Procura, sem data informada, os instantes em que a série tem um efeito que o
modelo não explica: o procedimento de Chen e Liu (1993), o mesmo do
`tsoutliers::tso()`.

1. Ajusta um ARIMA (o do **modelo** ligado ou, sem ele, um automático) e
   calcula os resíduos.
2. Em cada instante e para cada tipo, estima o efeito que haveria ali e a
   estatística t dele. Cada tipo deixa uma assinatura diferente nos
   resíduos: o inovacional, um resíduo grande isolado; o pulso, um resíduo
   grande seguido do eco dos pesos π do modelo; degrau e temporária, os
   seus.
3. Marca o maior |t| acima do **Valor crítico**, remove o efeito, reajusta e
   procura de novo.
4. Estima tudo junto e descarta os que deixaram de passar.

Os tipos: **pulso** (outlier aditivo, AO), **degrau** (mudança de nível, LS),
**temporária** (TC, um pulso que se desfaz à razão δ = 0,7 por período) e
**inovacional** (IO). O padrão são os três primeiros, como no `tsoutliers`.

### É exploratório

São dezenas de testes (cada instante, cada tipo), e por isso o valor crítico
é alto (3 a 4). Ainda assim, numa série longa aparecem candidatos por acaso.
Cada data é uma hipótese — "o que aconteceu em 1913?" — e não uma
intervenção confirmada. A que tiver explicação entra no modelo por
`series/intervencao`: a tabela liga direto na entrada **datas** dele (filtre
antes as linhas que não quer), e a temporária vira pulso com dinâmica
gradual. O p-valor que sair dali é otimista, porque a data veio do próprio
dado.

Um degrau achado aqui se confunde com raiz unitária: um modelo com
diferença demais absorve o degrau, e um com diferença de menos inventa
degraus. Confira com `series/zivot_andrews`.

### Valor crítico

0 (padrão) usa a regra do `tsoutliers`: 3 para séries de até 50
observações, 4 a partir de 450, e uma reta entre os dois.

### Faltantes

Este bloco não aceita faltantes: série com buraco põe o nó em vermelho. Ligue
um `series/interpolate` antes.

## Parâmetros

- **modelo** (entrada, opcional) — um `series/arima`; a busca usa a ordem
  dele (sem as intervenções declaradas). Sem ele, a ordem é escolhida pelo
  `auto.arima`.
- **Pulso (AO)**, **Degrau (LS)**, **Temporária (TC)**, **Inovacional (IO)** —
  que tipos procurar.
- **Valor crítico** — o |t| mínimo; 0 = a regra pelo tamanho da série.

## Valor

Uma tabela, uma linha por candidato, na ordem do tempo: `data`, `tipo`,
`sigla` (AO, LS, TC, IO), `indice` (a posição na série), `efeito` (o ω
estimado, na unidade da série) e `t`. Sem candidatos, a tabela vem vazia.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("nilo", "series/example", dataset = "Nile") |>
  tr_add("busca", "series/detect_interventions", from = "nilo")
```

## Veja também

`series/intervencao` para declarar o que for explicado; `series/pettitt` e
`series/zivot_andrews` para testar uma única quebra; `series/residuals` para
ver os resíduos do modelo.

