---
title: Margem com indicação
description: Se cada participante indicar k pessoas, quanto a margem melhora em cada nível?
section: colecoes
collection: amostragem
node: sampling/referral
related: [sampling/margin_levels, sampling/plot_margins, sampling/question_margins]
---

## O que o bloco faz

A expansão por indicação: cada participante da coleta de base convida k
pessoas. Para cada combinação de **convidados** e **ICC**, a tabela refaz as
margens de `sampling/margin_levels` com o n multiplicado.

Cada participante vira uma REDE de m = 1 + k · adesão respostas. Quem indica
indica parecido — amigos, colegas, vizinhos —, e respostas da mesma rede trazem
menos informação nova do que o mesmo número de pessoas independentes. O custo é
o deff de conglomerado, 1 + (m − 1) · ICC, que multiplica o deff da
ponderação:

- com ICC = 0, k convidados multiplicam o n efetivo por 1 + k;
- com ICC = 0,2 e 5 convidados, o n cresce 6 vezes mas o efetivo só 3.

O ICC real de uma rede de indicação só se mede depois da coleta (é a
correlação das respostas dentro das redes). Antes, trabalhe com cenários: 0,05
para perguntas pouco ligadas ao círculo social, 0,2 ou mais para as muito
ligadas (hábitos compartilhados, território).

Os cenários com 0 convidados são a coleta de base, na mesma tabela, para a
comparação direta.

## Quando usar

Use para avaliar como rodadas de indicação afetam o alcance e a margem nominal em unidades agrupadas.

## Configuração

- **Convidados por participante** — lista de k, separados por vírgula.
- **Adesão dos convidados** — a fração dos convidados que de fato responde.
- **ICC (cenários)** — lista de correlações intraclasse, com ponto decimal.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.data", registry = reg)
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("unidades", "sampling/example", dataset = "escolas_resumo") |>
  tr_add("com_n", "data/mutate", name = "n", expr = "10", from = "unidades") |>
  tr_add("margens_indicacao", "sampling/referral", unidade = "escola", tamanho = "alunos",
         n = "n", grupos = "rede", convidados = "0, 2, 4", icc = "0.1", from = "com_n")
```

## Como interpretar

A tabela preserva cada combinação de unidade, nível de indicação e ICC. A margem é nominal: o cálculo mede precisão sob suposições de amostragem, não corrige viés de seleção por indicação. A saída é Tabela `data/table` com cenários de indicação, ICC e margens.

## Veja também

`sampling/margin_levels`, `sampling/plot_margins`, `sampling/question_margins`.
