# trama.sampling (desenvolvimento)

## Mudanças de método (revisão metodológica, 2026-09-25)

- `sampling/proportion` (versão 2): o intervalo padrão passa a ser **logit**
  (Wald na escala log-odds, EP pelo método delta, t nos gl do desenho), o padrão
  de `survey::svyciprop` e o recomendado para amostras complexas (Korn &
  Graubard 1999, doi:10.1002/9781118032619): não sai de (0, 1) e é assimétrico
  perto dos extremos. Novo param `intervalo`: `logit`, `wilson` (escore de
  Wilson 1927 com n efetivo e t) ou `wald` (o intervalo da versão 1). A
  `margem` passa a ser a maior das duas metades. Validado contra
  `survey::svyciprop(method = "logit")` em AAS, estratificada, conglomerados e
  domínio (estimativa ≤ 1e-8, limites ≤ 1e-6).
