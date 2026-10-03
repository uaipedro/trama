---
title: Proporção
description: Estima a proporção da população em cada categoria de uma variável, com o erro do desenho.
section: colecoes
collection: amostragem
node: sampling/proportion
related: [sampling/size_proportion, sampling/plot_estimates, sampling/mean]
---

## O que o bloco faz

A proporção da população em cada categoria: a média ponderada do indicador
(1 se a unidade é da categoria, 0 se não). Com **Categoria** em branco, uma
linha por categoria; com uma categoria, só ela.

O card mostra em %; a tabela, em proporção (0 a 1).

O intervalo padrão é o **logit** (Wald na escala log-odds, t com os gl do
desenho), o padrão de `survey::svyciprop`: fica em (0, 1) e é assimétrico perto
dos extremos. **wilson** usa o escore de Wilson com o n efetivo do desenho;
**clopper_pearson** é o Clopper-Pearson com n efetivo ajustado pelos gl (Korn &
Graubard 1998); **wald** é p̂ ± t·EP.

Com proporção 0 ou 1 o erro padrão é zero: logit, wilson e clopper_pearson usam
então o Clopper-Pearson de Korn & Graubard com o n nominal do domínio, ajustado
pelos gl — numa AAS sem correção finita é o intervalo exato de `binom.test`
(0 em 150: [0; 2,4%]). Com conglomerados, o n nominal não desconta a
correlação interna e o intervalo pode ser curto demais. O wald fica no ponto.

## Quando usar

Use para estimar a fração populacional de uma categoria, uma ou todas as categorias de uma coluna.

## Configuração

- **Variável** — coluna categórica (texto, fator ou lógica).
- **Categoria** — o valor cuja proporção se quer; em branco, todas.
- **Intervalo** — `logit` (padrão), `wilson`, `clopper_pearson` ou `wald`.

## Exemplo

O fluxo usa os conjuntos didáticos registrados em `sampling/example` e inclui o nó desta página.

```r
library(trama)

reg <- tr_registry()
tr_use("trama.sampling", registry = reg)

tr_flow(reg) |>
  tr_add("pop", "sampling/example", dataset = "fazendas") |>
  tr_add("amostra", "sampling/stratified", estrato = "regiao", n = 240L, from = "pop") |>
  tr_add("proporcao", "sampling/proportion", variavel = "irrigada", nivel = "sim",
         por = "regiao", from = "amostra")
```

## Como interpretar

A tabela registra proporções entre 0 e 1, embora o card as apresente em porcentagem. Com categoria vazia, sai uma linha para cada categoria; intervalos de Wald podem ultrapassar 0 ou 1 em amostras pequenas. A saída é Estimativa `sampling/estimate` da proporção.

## Veja também

`sampling/size_proportion`, `sampling/plot_estimates`, `sampling/mean`.
