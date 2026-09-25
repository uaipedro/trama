# trama.ml (desenvolvimento)

## Correções de método

- `ml/roc` (versão 2): com `positiva` vazia, a classe positiva passa a ser a
  do nome da coluna de probabilidade (`.prob_<classe>`); se o nome não indicar
  uma classe observada, o segundo nível do fator (ordem alfabética para texto,
  a convenção de `glm` binomial). Antes era a segunda classe na ordem das
  linhas, sem relação com a coluna: com `y = sim, nao, sim, nao` e
  `.prob_sim = .8, .1, .7, .4` a AUC saía 0 em vez de 1 (curva espelhada;
  Fawcett 2006). Validação: AUC igual a U/(n1·n0) de Mann-Whitney
  (Hanley & McNeil 1982), conferida contra `stats::wilcox.test`.
