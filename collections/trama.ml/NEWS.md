# trama.ml 0.5.0

Versão sobe de 0.4.0 para 0.5.0: com a coesão das coleções os ajustes saem
como `models/fit`, os parâmetros seguem o glossário (`resposta`,
`preditores`) e prever/avaliar/importância são os blocos da `trama.models`.

## Integração 9.1b (coesão × main)

- Proveniência do `ml/split` (main): continua aqui; a recusa de prever o
  teste que o modelo viu roda no `tr_models_predict_raw()` da ml, e a recusa
  de avaliar o treino nos avaliadores da models (`permitir_treino`).
- `ml/forest` com `importancia` (impureza, permutação, impureza corrigida),
  lida no `models/importance`, com a medida descrita.
- DeLong/Youden da `ml/roc` e a precisão NA da `ml/evaluate` foram para
  `models/roc` e `models/evaluate`.

## 0.4.0

Versão sobe de 0.3.0: a revisão quebrou o isolamento do teste de 0.3.0 e as
correções mudam resultados. Blocos que mudam de comportamento sobem de
versão: `ml/split` 3, `ml/predict` 3, `ml/evaluate` 4, `ml/confusion` 3,
`ml/roc` 5, `ml/pr_curve` 3, `ml/tune` 4, `ml/nested_cv` 3,
`ml/importance` 2, `ml/cart` 4 e os demais modelos 3. Nova dependência:
`cli` (>= 3.6.0), pelo xxHash64 vetorizado.

## Proveniência por linha

- `ml/split` grava na marca das duas saídas as impressões digitais das
  linhas do teste: xxHash64 do conteúdo de cada linha nas colunas da divisão
  (números pelo valor binário exato), como multiconjunto — cópias no teste e
  cópias idênticas que o sorteio pôs no treino. Todo modelo guarda as
  impressões do seu treino, qualquer que seja a marca da entrada.
- `ml/predict` recusa (`tr_ml_error_test_leak`) prever linhas do teste que o
  modelo viu: ajuste na tabela inteira antes de dividir, numa cópia sem marca
  do teste ou no treino de outra divisão (B1).
- Ajuste, `ml/tune`, `ml/nested_cv` e nova divisão recusam uma tabela que
  junta treino e teste (`rbind`, `bind_rows`, `data/bind_rows`), mesmo com a
  marca de treino (B2).
- Os avaliadores recusam (`tr_ml_error_train_eval`) uma tabela marcada como
  teste que traz linhas de fora dele — previsões do treino juntadas às do
  teste, ou o teste repetido — salvo `permitir_treino` (B4).
- Linhas repetidas legítimas dos dois lados não disparam: a contagem de cópias
  as separa de vazamento.
- Custo medido em 100 mil linhas: ~1 s para as impressões e ~7 MB no modelo;
  os folds internos de `ml/tune` e `ml/nested_cv` não as recalculam.
- O que a marca não cobre, por construção, está documentado (B3): juntar com
  o teste à direita, remodelar (`pivot_longer`/`pivot_wider`), recriar a
  tabela à mão, reescrever ou tirar colunas da divisão e dividir fora do
  `ml/split`.

## Avaliação

- `ml/roc`: com AUC = 0 ou 1 a variância de DeLong é zero; o IC sai NA com a
  explicação em `auc_nota`, em vez de um intervalo de largura zero. Com menos
  de duas linhas numa classe, o IC também sai NA com nota (a curva e a AUC
  seguem; o `multi/roc` faz o mesmo).
- `ml/evaluate`: precisão de classe nunca prevista é indefinida (0/0): sai NA
  e fica fora das médias macro e ponderada (pesos renormalizados), como
  `zero_division = np.nan` do scikit-learn. Antes valia 0 (o padrão do
  scikit-learn), o que puxava as médias para baixo.
- `ml/tune` e `ml/nested_cv` avisam, e guardam em `nota`, folds de validação
  com uma classe só (macro F1, kappa e acurácia balanceada degeneram). Nova
  estratégia `grupo_estratificado`: grupos inteiros distribuídos para
  equilibrar as classes entre os folds (critério do StratifiedGroupKFold).

## Importância

- `ml/importance` ganha a coluna `medida`, que diz o que cada número mede. Na
  floresta de classificação (floresta de probabilidade), a permutação é o
  aumento do erro de Brier do `ranger` — média de (1 − p da classe
  observada)² fora da bolsa —, não a queda de acurácia, como a documentação
  dizia; na regressão, do erro quadrático médio. O XGBoost é rotulado como
  Gain relativo (fração do ganho total, soma 1).

# trama.ml 0.3.0

Versão sobe de 0.2.0: o isolamento do teste passa a ser imposto. Blocos que
mudam de comportamento sobem de versão: `ml/split` 2, `ml/predict` 2,
`ml/evaluate` 3, `ml/confusion` 2, `ml/roc` 4, `ml/pr_curve` 2, `ml/tune` 3,
`ml/nested_cv` 2, `ml/cart` 3 e os demais modelos 2.

## Isolamento do teste por construção

- `ml/split` marca as saídas com o atributo `tr_ml_origem` (`papel` =
  `"treino"`/`"teste"` e `divisao`, id que depende só dos dados e das linhas
  sorteadas). Escolhemos atributo, e não coluna oculta: o tipo `data/table`
  guarda em RDS, que preserva atributos na ida e volta do store e na releitura
  do cache; subconjunto, `dplyr::filter` e colunas novas também os preservam; e
  os dados, o card e a exportação ficam iguais.
- Ajustar (`ml/<modelo>`, `ml/tune`, `ml/nested_cv`) ou dividir de novo a
  saída teste é recusado com `tr_ml_error_test_leak` — o vazamento
  treino-teste de Kaufman, Rosset, Perlich & Stitelman (2012,
  doi:10.1145/2382577.2382579). O modelo guarda a origem do treino.
- `ml/predict` propaga a marca e recusa o teste de outra divisão com um modelo
  ajustado no treino de uma divisão marcada (`tr_ml_error_split_mismatch`).
- `ml/evaluate`, `ml/confusion`, `ml/roc` e `ml/pr_curve` ganham
  `permitir_treino` (padrão `FALSE`): previsões das linhas de treino marcadas
  são recusadas com `tr_ml_error_train_eval`. Com `TRUE`, o resultado é o
  mesmo número de antes, com aviso e a nota “avaliação no treino é otimista”
  (coluna `nota`; legenda do gráfico). Erro por padrão, e não só aviso, porque
  num fluxo de blocos o aviso se perde no card e o número otimista segue
  adiante como se fosse do teste; medir o treino de propósito (comparar com o
  teste, ver sobreajuste) continua possível com uma escolha explícita.
- Tabelas sem a marca (divisão feita por fora, treino e teste juntados) seguem
  como antes; os pressupostos dizem que ali o isolamento depende do usuário. A
  validação interna do `ml/tune` mede os folds de um treino marcado sem
  recusa: a validação de cada fold não ajustou o modelo do fold.
- Validação: testes de cada caminho, inclusive no fluxo real com o store e a
  segunda execução lida do cache, e pelo `store`/`restore` do `data/table`.

# trama.ml 0.2.0

- Integração 9.2: a `ml/pr_curve` foi para a `trama.models` como
  `models/pr_curve` (com modelo ou tabela, como a `models/roc`); fluxos com o
  id antigo migram, e as contas não mudaram.

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
- Novo bloco `ml/pr_curve`: curva precisão-revocação com precisão média
  (AP = Σ ΔR·P, sem interpolação) e área com a interpolação de Davis &
  Goadrich (2006) integrada em forma fechada (Keilwagen, Grosse & Grau 2014);
  linha do acaso na prevalência (Saito & Rehmsmeier 2015). Validação: exemplo
  à mão de 5 linhas (AP 0,7556; área 0,7161 em forma fechada),
  `yardstick::average_precision` (1e-10) e `PRROC::pr.curve` `auc.integral`
  (1e-8), com empates.
- `ml/roc` (versão 3): intervalo de confiança da AUC por DeLong, DeLong &
  Clarke-Pearson (1988), parâmetro `confianca` (padrão 0,95), e corte de
  Youden (1950) marcado no gráfico; os dados do gráfico ganham `limiar`,
  `auc_ep`, `auc_inf`, `auc_sup`, `youden_limiar`, `youden_j` e `youden`. AUC
  inalterada. Validação: `pROC` (AUC 1e-10; IC e variância de DeLong 1e-8 em
  90/95/99%; sensibilidade, especificidade e J do `coords(best.method =
  "youden")` 1e-12, com empates); AUC = 1 dá erro-padrão 0.
- `ml/forest`: parâmetro `importancia` = `impureza` (padrão, resultado
  anterior, versão mantida), `permutacao` (Breiman 2001) ou
  `impureza_corrigida` (AIR; Nembrini, König & Wright 2018), lida pelo
  `ml/importance` (atributo `medida`). Motivo: a impureza favorece preditores
  com muitos valores (Strobl et al. 2007). Validação: chamada direta do
  `ranger` com os mesmos argumentos e semente, igual a 1e-12 nas três
  medidas (regressão) e na permutação da floresta de probabilidade.
