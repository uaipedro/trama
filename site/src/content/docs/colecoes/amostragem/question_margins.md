---
title: Margem por pergunta
description: A margem de erro de pior caso de cada pergunta do questionário, em cada nível e cenário.
section: colecoes
collection: amostragem
node: sampling/question_margins
related: [sampling/margin_levels, sampling/referral]
---

## O que o bloco faz

Cruza o QUESTIONÁRIO com os níveis de uma tabela de margens (de
`sampling/margin_levels` ou `sampling/referral`) e dá, para cada pergunta em
cada nível, a margem de erro no pior caso do TIPO dela. Os cenários de
indicação, se vierem, são mantidos.

A tabela de perguntas tem uma linha por pergunta (ou por item de uma bateria):

| coluna | o que é |
|---|---|
| `pergunta` | o texto ou o código da pergunta |
| `tipo` | `binária`, `única`, `múltipla`, `escala`, `numérica` ou `aberta` |
| `opcoes` | quantas alternativas (ou pontos da escala) |
| `base` | fração da amostra que responde: 1 para todos, menos nas condicionais |

O pior caso de cada tipo, do mais preciso para o menos — uma alternativa, todas
ao mesmo tempo, uma contra outra, todas as comparações:

- **binária** (sim/não) e **múltipla** (marque todas: cada opção é um sim/não
  próprio) — p = 0,5: `margem_pp`.
- **única** (uma entre k) — cada alternativa sozinha tem a mesma `margem_pp`;
  mas para que TODAS as k proporções da distribuição fiquem dentro da margem ao
  mesmo tempo, vale a `margem_simultanea_pp` (Thompson 1987), maior. É ela que
  sustenta frases como "a ordem das alternativas é esta".
- **uma alternativa contra outra** (única, escala e múltipla) — a
  `margem_diferenca_pp`, o DOBRO da margem de uma proporção. As duas
  alternativas vêm das mesmas pessoas e disputam a mesma resposta: quando o
  acaso põe gente a mais em A, tira de B, e numa diferença esse movimento
  contrário se soma. O pior caso (as duas com 50%) dá variância 1/n. É a
  margem de frases como "preço preocupa mais que agrotóxico", para UM par
  escolhido antes de olhar os dados.
- **todas as comparações de uma vez** — a `margem_todos_pares_pp`, com
  Bonferroni sobre os k(k − 1)/2 pares. É a de "a ordem das alternativas é
  esta" ou de procurar, depois de ver os dados, qual par difere.
- **escala** (Likert de k pontos) — as margens da única, e a
  `margem_media_escala`, em pontos da escala, com o pior desvio possível
  (k − 1)/2.
- **numérica** — tratada como sim/não por faixa; categorize antes.
- **aberta** — sem margem.

**A base é o que mais pesa.** Uma pergunta que só quem trabalha responde, numa
população em que metade trabalha, tem a margem de uma amostra com metade do
tamanho: √2 vezes pior. Uma que só 20% respondem, √5 vezes pior. A **não
resposta esperada** ("prefiro não responder") desconta de todas as fechadas.

`confiavel` marca as linhas com pelo menos 30 respondentes: abaixo disso a
aproximação normal da margem já não vale, e o número não deve ir para o
relatório.

## Quando usar

Use para traduzir o plano de amostra em margens para perguntas específicas do questionário e suas bases.

## Configuração

- **Coluna da pergunta**, **do tipo**, **do nº de opções**, **da base** —
  colunas da tabela de perguntas (as duas últimas podem ficar em branco).
- **Não resposta esperada** — fração de "prefiro não responder" nas fechadas.
- **Confiança** — 90%, 95% ou 99%.

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
  tr_add("margens", "sampling/margin_levels", unidade = "escola", tamanho = "alunos", n = "n",
         grupos = "rede", from = "com_n") |>
  tr_add("perguntas", "sampling/example", dataset = "perguntas_exemplo") |>
  tr_add("resultado", "sampling/question_margins", nao_resposta = 0.05, from = "perguntas") |>
  tr_link("margens", "resultado:margens")
```

## Como interpretar

A saída cruza pergunta, nível e cenário. `margem_pp` descreve uma proporção; medidas simultâneas, diferença entre alternativas e todos os pares respondem perguntas distintas. `confiavel` sinaliza bases com ao menos 30 respondentes. A saída é Tabela `data/table` com uma linha por pergunta, nível e cenário.

## Veja também

`sampling/margin_levels`, `sampling/referral`.
