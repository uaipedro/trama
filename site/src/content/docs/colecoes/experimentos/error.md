---
title: "Erro"
description: "Soma os termos, sorteia o resíduo (normal, Poisson, binomial ou gama) e fecha a coluna de resposta."
section: colecoes
collection: experimentos
node: experiments/error
category: "Planejar"
related: [experiments/effect, experiments/view, models/anova_split_plot, models/glm, models/glmer, models/lmer]
---

<!-- Gerado por tools/site/export-collection-pages.R a partir da ajuda do bloco. -->

## O que o bloco faz

Fecha a resposta: soma os termos dos `experiments/effect`, sorteia o resíduo e
escreve a coluna **Resposta**. Depois dele o plano vai direto aos nós de
`trama.models` (pelo adaptador para tabela), e `plano$analise` já traz a
análise sugerida com a resposta.

### Distribuição

- **normal** — y = Σ termos + ε, ε com desvio-padrão **sd** (ou um sd por
  nível de **sd por nível de**). A coluna `.ef_residuo` guarda ε.
- **poisson** — Σ termos é o preditor linear η; y ~ Poisson(e^η).
- **binomial** — y ~ Binomial(**ensaios**, logit⁻¹(η)); com mais de um ensaio
  sai também `<resposta>_fracassos`.
- **gama** — média e^η e **forma** k (variância média²/k).

### Só na normal

- **Correlação no indivíduo** — `simetria_composta` (corr. ρ entre quaisquer
  duas medidas) ou `ar1` (ρ^|i−j|, na ordem do `tempo`, ou do `periodo`) entre
  as medidas do mesmo `individuo`. Pede um plano com `individuo` (medidas
  repetidas, crossover).
- **Caudas pesadas** — ε = sd · t_gl · √((gl−2)/gl): a mesma variância, caudas
  de t (gl > 2).
- **Assimetria** — ε = sd · gama padronizada de forma 4/γ², que tem média 0,
  variância 1 e assimetria γ.

Com correlação e caudas (ou assimetria) juntas, a correlação é exata e a
distribuição marginal é aproximada.

### Parcelas perdidas

**Proporção perdida** apaga a resposta de round(p·N) unidades sorteadas ao
acaso (MCAR), em qualquer distribuição.

### A análise sugerida

Na normal sem covariável, o nó do plano com a resposta (param `resposta`, ou
a fórmula prefixada). Com covariável, `models/lm` (ou `models/lmer`, se a
análise do delineamento tem termo aleatório, `(1 | ...)`) com a covariável na
fórmula. Fora da normal, `models/glm` ou `models/glmer` com a família, pela
mesma regra: é a fórmula do delineamento que decide, e não os
`experiments/effect` — o bloco do DBC entra fixo mesmo quando o efeito dele
foi simulado aleatório. Como o `models/glmer` não tem gama, a gama com termo
aleatório na fórmula (o erro de parcela da subdividida, por exemplo) sugere o
GLM só dos fixos, com aviso.

## Parâmetros

- **Resposta** — nome da coluna (não pode existir no plano).
- **Distribuição** — `normal`, `poisson`, `binomial`, `gama`.
- **Desvio-padrão (normal)**; **sd por nível de** + **sd de cada nível**
  (`baixa = 1, alta = 3`) para heterocedasticidade proposital.
- **Ensaios (binomial)**, **Forma (gama)**.
- **Correlação no indivíduo**, **ρ**.
- **Caudas pesadas: gl da t** (0 = não), **Assimetria** (0 = não) — uma ou outra.
- **Proporção perdida** — 0 a 0,9.

## Valor

O plano (`experiments/plan`) com a coluna da resposta e `plano$resposta`
(distribuição, parâmetros, unidades perdidas, semente). Ligado num nó de
`trama.models`, vira a tabela de unidades.

## Exemplos

```r
tr_flow(reg) |>
  tr_add("plano", "experiments/design", estrutura = "parcela_subdividida",
         fatores = "irrigacao: baixa, alta; variedade: A, B, C", repeticoes = 4L) |>
  tr_add("mu", "experiments/effect", tipo = "intercepto", valor = 50, from = "plano") |>
  tr_add("bl", "experiments/effect", tipo = "aleatorio", fator = "bloco", sd = 3, from = "mu") |>
  tr_add("var", "experiments/effect", tipo = "fixo", fator = "variedade",
         efeitos = "A = 0, B = 1, C = 3", from = "bl") |>
  tr_add("ep", "experiments/effect", tipo = "aleatorio", fator = "bloco:parcela", sd = 4, from = "var") |>
  tr_add("y", "experiments/error", resposta = "producao", sd = 1, from = "ep") |>
  tr_add("sp", "models/anova_split_plot", resposta = "producao", parcela = "irrigacao",
         subparcela = "variedade", bloco = "bloco", from = "y")
```

## Veja também

`experiments/effect`; `experiments/view` (aba `componentes`);
`models/anova_split_plot`, `models/glm`, `models/glmer`, `models/lmer`.

### O sorteio

O bloco é **estocástico**: a semente é do card, e não da sessão. O mesmo
documento sorteia sempre os mesmos valores; trocar a semente sorteia outros.
A semente fica no plano (no termo ou em `plano$resposta`). A semente do console
(`set.seed()`) não é tocada. Para repetir a cadeia inteira com uma semente só,
use `tr_experiments_simulate()` no console.

