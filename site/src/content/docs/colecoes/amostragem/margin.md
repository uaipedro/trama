---
title: Margem para n dado
description: Com n entrevistas, qual a margem de erro de uma proporção?
section: colecoes
collection: amostragem
node: sampling/margin
related: [sampling/margin_levels, sampling/size_proportion]
---

## O que o bloco faz

O caminho contrário do `sampling/size_proportion`: o n já está dado (o campo
contratado, o orçamento fechado) e a pergunta é quanto se erra:

    E = z · √(deff · p(1 − p) / n · (1 − n/N))

O card mostra a margem em pontos percentuais e a escada: entrevistas previstas,
as que viram resposta, o n efetivo depois do deff. É a conta de uma célula
só; para vários níveis de uma vez, `sampling/margin_levels`.

## Quando usar

Use quando o n já foi definido e a pergunta é a margem esperada para uma proporção.

## Configuração

- **Entrevistas** — n previsto.
- **Proporção esperada** — p; 0,5 é o pior caso.
- **Confiança**, **População**, **Efeito do desenho**, **Taxa de resposta** —
  como em `sampling/size_proportion`.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("margem", "sampling/margin", n = 400L, proporcao = 0.5,
         confianca = 0.95, populacao = 5000, deff = 1.2)
```

## Como interpretar

O plano resume a margem em pontos percentuais e a escada de n previsto, respostas esperadas e n efetivo. Para n pequeno, a aproximação normal deve ser lida com cautela. A saída é Plano `sampling/plan` que resume a margem para o n informado.

## Veja também

`sampling/margin_levels`, `sampling/size_proportion`.
