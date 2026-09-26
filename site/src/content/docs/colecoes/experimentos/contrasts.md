---
title: "Contrastes"
description: "Abre o SQ do tratamento: uma linha por contraste (polinomiais, Helmert, controle, 2^k ou digitados), com a conferência da soma e da ortogonalidade."
section: colecoes
collection: experimentos
node: experiments/contrasts
category: "Analisar"
related: [models/linear_hypothesis, models/polinomial, models/emmeans, experiments/boxcox]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

**Tudo é contraste.** Com k tratamentos, o SQ do tratamento é a soma de k − 1
contrastes ortogonais; a tendência linear de doses, cada tratamento contra o controle, o
efeito principal e a interação de um fatorial são todos contrastes. Este bloco
lê um modelo já ajustado e abre a ANOVA: **uma linha por contraste**, com
estimativa Σ cᵢ ȳᵢ, SQ, F e p.

### Conjuntos

- **polinomiais** — linear, quadrático, cúbico… até o grau k − 1. Vale para
  níveis **desigualmente espaçados** e **réplicas desiguais**: os coeficientes
  saem de `poly()` sobre as observações (cᵢ = rᵢ pⱼ(xᵢ)), e não da tabela de
  níveis igualmente espaçados. No caso igualmente espaçado e balanceado, são os
  inteiros das tabelas (`-3 -1 1 3`, `1 -1 -1 1`, `-1 3 -3 1`).
- **helmert** — cada nível contra a média dos anteriores.
- **controle** — cada tratamento contra o controle (`B vs A`, `C vs A`…, as
  comparações de Dunnett), k − 1 contrastes. Não são ortogonais (todos usam a
  média do controle), então os SQ não somam o do tratamento; o p de cada linha
  é o de uma comparação, sem o ajuste de Dunnett para as k − 1 juntas.
- **fatorial 2^k** — os efeitos principais e as interações de 2 a 5 fatores de
  dois níveis, como contrastes de sinais ±1 nas médias das células (o SEGUNDO
  nível de cada fator é o alto, +1). A coluna
  `efeito` é a estimativa dividida por 2^(k−1): a diferença entre as médias do
  nível alto e do baixo.
- **digitados** — na mesma sintaxe do `models/linear_hypothesis` (números, um
  por nível, ou expressão nos nomes dos níveis, com rótulo opcional antes de
  `:`).

### O que se confere

- **Soma dos SQ**: no rodapé, Σ SQ dos contrastes ao lado do SQ do tratamento
  lido do quadro do próprio modelo, e se confere. Só se espera que some quando o
  conjunto tem k − 1 contrastes e é ortogonal.
- **Ortogonalidade**: a porta `ortogonalidade` traz a matriz Σ cᵢdᵢ/rᵢ, que é
  a covariância entre dois contrastes de médias a menos de σ². Fora da diagonal,
  zero é ortogonal; os pares que não são aparecem na nota — e é por isso que os
  SQ deixam de somar (eles se sobrepõem). Com réplicas desiguais, contrastes
  ortogonais no papel (Σ cᵢdᵢ = 0, como o Helmert) deixam de ser.
- **Regressão**: no conjunto polinomial sobre um `lm`, a coluna `sq_regressao`
  traz o SQ sequencial de cada grau de `poly(dose)` no mesmo modelo, e ele é
  igual ao SQ do contraste: o polinômio ortogonal É a regressão.

### Desdobramento da interação

Com **Dentro** preenchido, os contrastes do fator são estimados em cada nível do
outro fator (linear de N dentro de cada variedade). Com um conjunto completo e
ortogonal, a soma de todos os SQ desdobrados é SQ(fator) + SQ(fator × dentro).
Os coeficientes de cada grupo usam as réplicas da CÉLULA daquele grupo: com
células de tamanhos diferentes, os polinômios de cada nível de **Dentro** saem
diferentes e ortogonais dentro da célula, e a porta `ortogonalidade` traz uma
matriz por grupo.

### O termo de erro

O F de cada contraste é o t² do `emmeans` sobre o modelo, e é o modelo que
escolhe o erro: no delineamento e no `lm`, o resíduo; na **parcela
subdividida**, o misto do `models/fit` com gl de Satterthwaite — no balanceado,
o erro a para contrastes entre níveis da parcela e o erro b para os da
subparcela. O `qm_erro` de cada linha é SQ / F, o erro que aquela comparação de
fato usou. O SQ de cada linha é o SQ extra do teste de 1 gl, `sq` = F ×
`qm_erro` (o mesmo do `car::linearHypothesis`). No `lm` e nos delineamentos,
`qm_erro` é o QM do resíduo, igual em todas as linhas; no misto e na parcela
subdividida, é o erro efetivo (combinado) que aquele contraste usou. No
balanceado, `sq` é o SQ do livro, est² / Σ(cᵢ²/rᵢ), com rᵢ as observações de
cada média; no desbalanceado os dois diferem, e o do livro sai à parte, na
coluna `sq_livro`.

## Parâmetros

- **Fator** — o fator cujas médias se contrastam. No conjunto `fatorial 2^k`,
  de 2 a 5 fatores de dois níveis, separados por vírgula.
- **Conjunto** — `polinomiais`, `helmert`, `controle`, `fatorial 2^k` ou
  `digitados`.
- **Contrastes** — só no conjunto `digitados`: um por linha ou separados por
  `;`, cada um com rótulo opcional (`linear: -3 -1 1 3; B - A`).
- **Controle** — o nível controle no conjunto `controle`; em branco, o primeiro.
- **Doses** — os valores numéricos dos níveis, na ordem, para os polinomiais
  (`0 30 60 120`); em branco, os nomes dos níveis lidos como número.
- **Dentro** — um fator em cujos níveis os contrastes são desdobrados.

## Valor

Duas portas:

- `out` — um quadro de efeitos (`models/effects`) com uma linha por contraste:
  `termo`, `coeficientes`, `estimativa`, `erro_padrao`, `gl`, `sq`, `F`,
  `gl_erro`, `qm_erro`, `p_valor` (e `efeito` no 2^k, `sq_regressao` nos
  polinomiais sobre `lm`, `sq_livro` quando o modelo é desbalanceado). O rodapé traz a
  conferência da soma dos SQ.
- `ortogonalidade` — uma tabela (`data/table`) com a matriz Σ cᵢdᵢ/rᵢ entre os
  contrastes (uma por nível de **Dentro**, com a coluna do grupo).

## Exemplos

```r
tr_flow(reg) |>
  tr_add("aveia", "models/example", dataset = "aveia") |>
  tr_add("split", "models/anova_split_plot", resposta = "producao", parcela = "variedade",
         subparcela = "nitrogenio", bloco = "bloco", from = "aveia") |>
  tr_add("pol", "experiments/contrasts", fator = "nitrogenio", conjunto = "polinomiais",
         doses = "0 0.2 0.4 0.6", dentro = "variedade", from = "split")
```

## Veja também

`models/linear_hypothesis` para o F conjunto dos mesmos contrastes;
`models/polinomial` para a curva ajustada às doses; `models/emmeans` para as
médias que os contrastes combinam; `experiments/boxcox` quando os resíduos pedem
transformação.

### Como ler o card do teste

O topo diz o teste, as estrelas e a hipótese nula (**H0**). No meio, o número
grande e o **selo** da decisão ao nível de 5%: preenchido quando rejeita H0,
vazado quando não rejeita. Embaixo, a conclusão em uma linha.

**Quando o teste tem p-valor**, o número grande é o p-valor (abaixo de 1 em mil,
escrito em potência de dez) e embaixo dele vem a **régua**:

- a régua é o p-valor em escala logarítmica: quanto MAIS COMPRIDA a barra,
  MENOR o p-valor e mais forte a evidência contra H0. A ponta marca onde o
  p-valor está;
- as marcas são os cortes de 10%, 5%, 1% e 1 em mil; a barra enche de vez
  abaixo de 1 em dez mil;
- as **estrelas** são as do `summary()` do R: `***` abaixo de 1 em mil, `**`
  abaixo de 1%, `*` abaixo de 5%, `.` abaixo de 10% e `ns` acima. A cor da barra
  fica mais forte a cada estrela; sem estrela, cinza.

**Quando o teste só tem tabela de valores críticos** (sem p-valor), o número
grande é a estatística, e no lugar da régua vêm **três pontinhos**, dos cortes
de 10%, 5% e 1%:

- **preenchido** — a estatística passa do valor crítico daquele nível, na cauda
  que o teste usa;
- **na cor de destaque** — a decisão a 5% é rejeitar H0; **cinza** — não é: um
  ponto cinza preenchido é um teste que vence só o corte de 10%;
- **vazio** — não passa daquele corte.

Quantos pontos acendem diz a FOLGA da decisão, não uma decisão diferente.

A vista **detalhe** traz o registro inteiro: estatística, graus de liberdade,
p-valor ou valores críticos, o tamanho do efeito com o intervalo de 95%
desenhado contra a referência (quando o teste tem um), as partes de um teste
conjunto, a nota e a fonte.

Não rejeitar H0 não é provar H0: com poucas observações o teste deixa de
rejeitar por falta de poder. A conclusão diz "não há evidência", e não "é
igual", por isso.

