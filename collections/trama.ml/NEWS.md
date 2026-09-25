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
- `ml/cart` (versão 2): poda por custo-complexidade com a regra 1-EP sobre a
  validação cruzada de 10 folds do `rpart` (Breiman et al. 1984, sec. 3.4.3):
  a menor subárvore com `xerror` ≤ mínimo + 1 erro-padrão. Novos parâmetros
  `cp` (padrão 0) e `poda` (`"1ep"`, `"minimo"`, `"nenhuma"`; `"nenhuma"`
  reproduz a árvore sem poda da versão 1). O `cptable`, o CP e o número de
  divisões escolhidos ficam em `extras$poda`. `ml/tune` (versão 2) herda a
  poda ao ajustar CART. Validação: `rpart::printcp`/`prune` reproduzidos em
  `airquality` (casos completos) com semente 42 — árvore cheia de 30 divisões,
  1-EP com 3 divisões, previsões idênticas.
- `ml/linear`: parâmetro `corte` (padrão 0,5, que reproduz o comportamento
  anterior — versão do nó mantida) para a logística binária: prevê a segunda
  classe quando P ≥ `corte`. Validação: `.prob_*` iguais a `fitted(glm)` e
  `.pred` igual à regra aplicada à mão para cortes 0,3, 0,5 e 0,8.
