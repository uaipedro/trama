# trama.ml 0.2.0

Versão sobe de 0.1.0: mudam padrões e resultados de `ml/roc` (classe
positiva) e `ml/cart` (poda), com os nós em versão 2.

## Correções de método

- `ml/roc` (versão 2): com `positiva` vazia, a classe positiva passa a ser a
  do nome da coluna de probabilidade (`.prob_<classe>`); se o nome não indicar
  uma classe observada, o bloco recusa com `tr_ml_error_positive_required` e
  pede `positiva` (adivinhar pelo segundo nível espelharia a curva quando a
  coluna for da outra classe). Antes era a segunda classe na ordem das
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
  1-EP com 3 divisões, previsões idênticas. Com n < 10, a validação cruzada usa
  min(10, n) folds (deixa-um-fora), número guardado em `extras$poda$folds`;
  conferido contra `rpart` com `xval = 1:n` (cptable igual a 1e-12 com n = 3,
  5 e 8).
- `ml/linear`: parâmetro `corte` (padrão 0,5, que reproduz o comportamento
  anterior — versão do nó mantida) para a logística binária: prevê a segunda
  classe quando P ≥ `corte`. Validação: `.prob_*` iguais a `fitted(glm)` e
  `.pred` igual à regra aplicada à mão para cortes 0,3, 0,5 e 0,8.
- `ml/svm` / `ml/predict`: `.pred` passa a ser calculado explicitamente como a
  classe de maior `.prob_*` (empates na ordem dos níveis). A revisão
  metodológica supunha que `.pred` vinha da margem e podia discordar das
  probabilidades; na verdade, com `probability = TRUE` o
  `svm_predict_probability` do LIBSVM (Chang & Lin 2011) já rotula pela maior
  probabilidade de Platt — o pressuposto antigo estava errado e foi corrigido.
  Resultados idênticos fora de empates exatos, por isso a versão do nó foi
  mantida. Teste: `.pred` igual ao argmax das `.prob_*` em iris binária
  (onde a regra da margem discorda em 2 linhas) e em iris com 3 classes.

## Novos recursos

- `ml/split` e `ml/tune`: `estrategia` = `aleatoria` (padrão, resultado
  idêntico ao anterior, versões mantidas), `temporal` (coluna `ordem`) ou
  `grupo` (coluna `grupo`). Divisão temporal: treino = linhas até o instante da
  linha ⌊n·p⌋ na ordem do tempo, empates do mesmo lado; por grupo: ⌊G·p⌋ grupos
  inteiros. Validação cruzada temporal por origem móvel com janela crescente
  (Tashman 2000; Hyndman & Athanasopoulos, FPP3, sec. 5.10; Bergmeir, Hyndman
  & Koo 2018): os instantes formam `folds + 1` blocos contíguos e o fold i
  treina nos blocos 1..i e valida no i + 1; por grupo, cada grupo num só fold
  (Roberts et al. 2017). Com `cols` vazio, `ordem` e `grupo` não viram
  preditores. Validação: contabilidade exata dos folds (20 dias em 5 blocos de
  4; cada linha valida uma vez por grupo), nenhum grupo/instante futuro no
  treino, datas e números dão o mesmo corte.
- Novo bloco `ml/nested_cv`: validação cruzada aninhada (Varma & Simon
  2006). Cada fold externo roda o `ml/tune` completo só no seu treino e mede o
  vencedor na validação externa; saída com a métrica interna (otimista) e a
  externa por fold, e a média. Validação: fold externo refeito à mão (mesmos
  folds e semente) e, em ruído puro (12 réplicas semeadas, n = 40, SVM, 10
  tentativas), aninhada 0,519 de acurácia (acaso 0,5; tolerância 0,06) contra
  0,604 da média do vencedor não aninhada.
- `ml/evaluate` (versão 2): na classificação, além de acurácia, acurácia
  balanceada e macro F1 (valores inalterados), kappa de Cohen (1960),
  precisão e revocação macro, precisão/revocação/F1 ponderados pelo suporte e
  precisão/revocação/F1 por classe; nova coluna `classe` (a tabela ganha
  linhas). Precisão de classe nunca prevista vale 0. `ml/tune` e
  `ml/nested_cv` aceitam `kappa` e `weighted_f1` como métrica. Validação:
  exemplo à mão de 3 classes (kappa 0,5; F1 0,8/0,5/0,667) e `yardstick`
  (kap, f_meas/precision/recall macro e macro_weighted, bal_accuracy binária)
  a 1e-12.
