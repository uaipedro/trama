---
title: Escolher um gráfico
description: Escolha a forma pela pergunta e pela unidade representada em cada marca.
section: colecoes
collection: visualizacao
order: 1
related: [view/points, view/histogram, view/bars]
---

## A pergunta orienta a forma

A coleção Visualização transforma tabelas em gráficos. Antes de escolher a forma, identifique o que cada linha representa e se o gráfico deve preservar cada observação ou resumir grupos.

| Pergunta | Nós | Leitura principal |
| --- | --- | --- |
| Duas medidas variam juntas? | `view/points`, `view/labels`, `view/bin2d` | Uma marca por linha, uma linha rotulada ou contagens numa grade |
| Como uma medida se distribui? | `view/histogram`, `view/density`, `view/ecdf`, `view/qq` | Contagens em faixas, curva suavizada, proporção acumulada ou comparação com a normal |
| Como a distribuição varia entre grupos? | `view/boxplot`, `view/violin`, `view/strip` | Quartis, forma ou observações individuais |
| Como séries evoluem ou compõem um total? | `view/line`, `view/area` | Trajetórias ou composição empilhada em eixo ordenado |
| Como categorias se comparam? | `view/bars`, `view/means`, `view/dotplot`, `view/dumbbell`, `view/paired`, `view/pareto` | Contagens e somas, médias e incerteza, ranking, condições ou acumulado |
| Como dois grupos de categorias se relacionam? | `view/heatmap` | Contagem ou soma em cada par de categorias |
| O que acrescentar a um gráfico pronto? | `view/reference`, `view/fit_line`, `view/annotate` | Linha ou faixa de referência, reta ajustada com IC e equação, texto com seta |
| Como levar os gráficos ao artigo? | `view/combine`, `view/save` | Painel com etiquetas A, B, C e um tema só; arquivo no tamanho e na resolução do periódico |

## Dados antes da aparência

Os gráficos que contam, somam ou calculam médias declaram essa agregação na página do bloco. Para médias por grupo, use `view/means` com as observações originais; para uma estatística diferente, calcule-a antes com `data/group_summarise`. Em `view/points`, cada linha vira uma marca, então a sobreposição pode esconder observações; `view/bin2d` conta pontos em células quando a nuvem é grande.

Eixo em log exige valores positivos. Em eixos ordenados, confirme que a ordem das categorias corresponde à pergunta; fatores permitem definir essa ordem. Título, rótulos e tema ajudam a ler o resultado, mas não substituem a escolha da unidade e da transformação.
