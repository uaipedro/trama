---
title: Escolher um gráfico
description: Comece pela pergunta e pela estrutura da tabela, não pelo tipo de gráfico mais familiar.
section: colecoes
collection: visualizacao
order: 1
related: [view/points, view/histogram]
---

## A pergunta orienta a forma

A coleção Visualização produz gráficos `ggplot2` a partir de tabelas. O bloco
adequado depende da pergunta que a análise precisa responder e de como uma
linha da tabela deve aparecer no gráfico.

| Pergunta | Bloco | Uma linha representa |
| --- | --- | --- |
| Duas medidas variam juntas? | `view/points` | Uma observação |
| Como uma medida se distribui? | `view/histogram` | Uma observação em uma faixa |
| Grupos diferem? | `view/boxplot` | Uma observação dentro de um grupo |
| Qual categoria tem maior valor? | `view/bars` | Uma categoria ou medida agregada |

> **Antes de continuar**
>
> Um gráfico não corrige a estrutura dos dados. Se cada barra deve representar
> uma média por região, essa média é calculada antes com `data/group_summarise`.

## Dados antes da aparência

Título, rótulos e tema melhoram a leitura, mas a decisão principal é o vínculo
entre uma linha da tabela e uma marca no gráfico. Confira a tabela de entrada
antes de configurar cor, tamanho ou legenda.
