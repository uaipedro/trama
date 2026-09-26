---
title: "Poder"
description: "Repete a cadeia effect → error e a análise N vezes e conta as rejeições: poder (ou erro tipo I) com IC, e a curva por nº de repetições."
section: colecoes
collection: experimentos
node: experiments/power
category: "Avaliar"
related: [experiments/effect, experiments/error, experiments/contrasts, experiments/randomization_test, models/anova_table, models/anova_split_plot]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Poder por **simulação de Monte Carlo**, sem fórmula fechada por delineamento:
o bloco refaz a cadeia que o plano guarda — o mesmo `experiments/design`, cada
`experiments/effect` com os seus argumentos e o `experiments/error` —,
**Réplicas** vezes com sementes derivadas, roda a análise em cada resposta
simulada e conta em quantas o p do teste ficou abaixo de **Significância**.
A taxa sai com o IC exato de **Clopper-Pearson** no nível **Confiança**.

O que a taxa é depende do modelo declarado, e a tabela diz qual é:

- **H0 falsa** (o termo tem efeito na parte fixa declarada): a taxa é o
  **poder**.
- **H0 verdadeira** (efeito declarado zero): a taxa é o **erro tipo I**, que
  deve cair perto de α. Se não cai, a análise usa o erro errado: na parcela
  subdividida com erro de parcela, a análise ingênua (`models/anova_factorial`,
  que testa a parcela contra o resíduo das subparcelas) rejeita muito mais que
  5%, e a de parcela subdividida fica em 5%.

A conferência de H0 lê a parte fixa da resposta (intercepto, fixo, interação,
quantitativo): médias por nível iguais no efeito principal, médias de célula
aditivas na interação, Σ cᵢ mᵢ = 0 no contraste. Termo aleatório e covariável
não entram nela.

### Curva de poder

**Grade de repetições** (`3, 4, 6, 8`) refaz o delineamento com cada valor no
param `repeticoes` do `experiments/design` (repetições no DIC, blocos no DBC)
e desenha a taxa contra ele. As sementes das réplicas são as mesmas em todo
ponto da grade (números aleatórios comuns): a curva fica mais lisa, e cada
ponto continua estimando o poder daquele tamanho.

### Custo

Um ajuste por réplica e ponto da grade: 200 réplicas de uma ANOVA levam
poucos segundos. O erro de Monte Carlo da taxa é √(p(1 − p)/R): com R = 200 e
p = 0,5, 0,035; o IC já o mostra.

## Parâmetros

- **Análise (nó de models)** — em branco, a do plano (`plano$analise`, a que o
  `experiments/error` sugeriu). Outro id (`models/anova_factorial`) troca a
  análise; os params dela vêm de **Params da análise**.
- **Params da análise** — `nome = valor; nome = valor`, sobrescrevendo os do
  plano. A resposta é posta sozinha quando o nó tem `resposta`.
- **Termo testado** — a linha do quadro de ANOVA (`models/anova_table`, SQ
  tipo I) cujo p conta: `irrigacao`, `irrigacao:variedade`. Em branco, o
  primeiro fator de tratamento do plano.
- **Contraste: conjunto** — `nenhum` testa o F do termo; um conjunto do
  `experiments/contrasts` testa o F (1 gl) de uma linha dele, com o **Termo**
  como fator contrastado.
- **Contraste: linha** — o nome da linha (`linear`, `quadratico`).
- **Contrastes (digitados)**, **Controle**, **Doses** — como no
  `experiments/contrasts`.

- **Réplicas** — respostas simuladas por ponto da grade.
- **Significância (α)** — rejeita quando p < α.
- **Confiança do IC** — nível do intervalo de Clopper-Pearson da taxa.
- **Grade de repetições** — valores do param `repeticoes` do design; em
  branco, só o tamanho do plano.

## Valor

`out`: o gráfico da taxa de rejeição (ponto e IC; linha na grade), com α
tracejado. `tabela`: uma linha por ponto da grade — `repeticoes`, `unidades`,
`replicas` (as que ajustaram), `falhas`, `rejeicoes`, `taxa`, `li`, `ls`,
`confianca`, `significancia`, `p_binomial`, `hipotese` (H0 verdadeira ou
falsa no modelo declarado), `analise` e `teste`. Com H0 verdadeira,
`p_binomial` é o p do teste binomial exato de "taxa = α" (`binom.test(rejeicoes,
replicas, p = significancia)`): p pequeno diz que o teste não mantém o tipo I
nominal, e não ruído de Monte Carlo (o critério de Oliveira & Ferreira, 2010);
com H0 falsa, NA.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "dic", fatores = "t: A, B, C, D", repeticoes = 4L) |>
  tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 10, from = "plano") |>
  tr_add("t", "experiments/effect", tipo = "fixo", fator = "t", efeitos = "A = 0, B = 0, C = 1, D = 2",
         from = "mu") |>
  tr_add("y", "experiments/error", sd = 1, from = "t") |>
  tr_add("poder", "experiments/power", replicas = 20L, repeticoes = "3, 5", from = "y")
```

## Veja também

`experiments/effect`, `experiments/error`, `experiments/contrasts`,
`experiments/randomization_test`, `models/anova_table`,
`models/anova_split_plot`.

### Aparência (comum a todos os gráficos)

- **Proporção** — a forma da imagem: `16:9` e `2:1` para paisagem, `1:1` para
  quadrado, `4:3` e `3:4` para o que vai numa página. A imagem sai sempre com
  1600 px no lado maior; o card só a escala, e um clique a abre em tela cheia.
  Mudar a proporção RECOMPUTA o gráfico — é o único param de aparência que
  muda mesmo o desenho, porque o ggplot recoloca a legenda e remede os rótulos.
  Arrastar a alça do card, não: aquilo é tamanho, não proporção.
- **Tema** — `padrão` segue o tema padrão do projeto; os temas (fundo, cores,
  fonte, paleta) são do projeto, e não do gráfico. Sem temas próprios valem os
  embutidos, com `escuro` como padrão; `claro` e `clássico` servem bem ao que
  sai no relatório. O tema dos gráficos não segue o claro/escuro do editor, que
  é preferência de cada pessoa: para mudar todos de uma vez, troque o tema
  padrão em ⚙ Configurações. A paleta só entra onde o
  gráfico não escolheu cores por conta própria.
- **Título**, **Rótulo do X**, **Rótulo do Y** — em branco, o gráfico usa o
  nome da coluna, que costuma ser a legenda certa.
- **Legenda** — `nenhuma` quando a cor já está explicada no título.


### O sorteio

O bloco é **estocástico**: a semente é do card, e não da sessão. O mesmo
documento sorteia sempre os mesmos valores; trocar a semente sorteia outros.
A semente fica no plano (no termo ou em `plano$resposta`). A semente do console
(`set.seed()`) não é tocada. Para repetir a cadeia inteira com uma semente só,
use `tr_experiments_simulate()` no console.

