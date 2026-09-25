# trama.experiments 0.1.0

* Primeira versão: o nó `experiments/design` declara e sorteia DIC, DBC,
  quadrado latino, fatorial (em DIC ou DBC), fatorial com confundimento em
  blocos, fatorial fracionado 2^(k−p), composto central, parcelas
  subdivididas, faixas, blocos incompletos balanceados, medidas repetidas,
  crossover de Williams e grupos de experimentos; o tipo `experiments/plan`
  vira tabela para os nós de ANOVA da `trama.models`; `experiments/view`
  desenha o mapa, a hierarquia, as combinações e a ordem de execução.
* `experiments/contrasts` (versão 2): no desbalanceado, `sq` passa a ser o SQ
  extra do teste de 1 gl (F × `qm_erro`, o do `car::linearHypothesis`) e
  `qm_erro` o QM do resíduo; o SQ de livro vai para `sq_livro`. Com `dentro` e
  células desiguais, os polinômios de cada grupo usam as réplicas da célula.
* `experiments/response_surface` (versão 2): com bloco, o erro puro é o resíduo
  de `y ~ bloco + ponto`, como no `rsm`.
* `experiments/boxcox` (versão 2): λ̂ na borda da grade é dito borda
  (`na_borda`), sem transformação sugerida.
