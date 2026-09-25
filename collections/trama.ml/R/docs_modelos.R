# Pressupostos e referências: os blocos que ajustam um modelo (`ml/fit`).

.tr_ml_docs_modelos <- function() {
  P <- .tr_ml_P; I <- .tr_ml_impl; R <- trama::tr_ref
  L <- .tr_ml_livros(); C <- .tr_ml_comuns()
  base <- list(C$so_treino, C$sem_vazamento, C$iid)
  num <- P("Os preditores são **numéricos, finitos e sem faltantes** — o bloco recusa o resto. Fatores entram só depois de codificados em números.",
    verificar = "data/summary",
    se_falhar = "Trate faltantes com o `data/drop_na` ou o `data/replace_na` e codifique fatores com o `data/mutate`. Uma imputação que usa estatísticas (média, mediana) deve sair só do treino.")
  imp_arvore <- function(medida) P(sprintf(
    "A **importância** (no `ml/importance`) é %s, medida no treino: descreve o ajuste, não o efeito. Tende a favorecer preditores contínuos ou com muitos valores distintos, e preditores correlacionados repartem entre si a importância.", medida),
    verificar = "data/summary",
    se_falhar = "Leia a importância como resumo do modelo, não como ranking de causas. Importância por permutação ou condicional ainda sem bloco no trama.")
  breiman01 <- R(autores = "Breiman, L.", ano = 2001, titulo = "Random Forests",
                 fonte = "Machine Learning, 45(1), 5-32", doi = "10.1023/A:1010933404324")
  cart84 <- R(autores = c("Breiman, L.", "Friedman, J. H.", "Olshen, R. A.", "Stone, C. J."), ano = 1984,
              titulo = "Classification and Regression Trees",
              fonte = "Belmont: Wadsworth; reimpressão New York: Routledge, 2017",
              doi = "10.1201/9781315139470")
  strobl <- R(autores = c("Strobl, C.", "Boulesteix, A.-L.", "Zeileis, A.", "Hothorn, T."), ano = 2007,
              titulo = "Bias in random forest variable importance measures: Illustrations, sources and a solution",
              fonte = "BMC Bioinformatics, 8, 25", doi = "10.1186/1471-2105-8-25", papel = "complementar")
  list(
    "ml/linear" = list(
      pressupostos = c(base, list(num,
        P("A relação entre os preditores e a resposta (ou o logito da classe, na logística) é **linear e aditiva**: sem curvatura nem interação que o modelo não tenha.",
          verificar = "ml/residuals",
          se_falhar = "Crie termos (quadrado, produto) com o `data/mutate` ou compare com um modelo de árvore (`ml/cart`, `ml/forest`) no mesmo teste."),
        P("Na classificação, **duas classes**, e a classe prevista é a de probabilidade ≥ 0,5. Com classes desequilibradas esse corte favorece a maioritária.",
          verificar = "ml/confusion",
          se_falhar = "Leia a `balanced_accuracy` no `ml/evaluate` e a curva no `ml/roc`, que não dependem do corte. Escolher outro corte ainda sem bloco no trama."))),
      referencias = list(L$islr, L$esl,
        I("stats", "lm", "Regressão: mínimos quadrados sobre os preditores escolhidos, sem interações."),
        I("stats", "glm", "Classificação binária: `family = binomial()`; a classe prevista é a de probabilidade ≥ 0,5."))),

    "ml/cart" = list(
      pressupostos = c(base, list(num,
        P("A árvore **não é podada**: cresce até `max_depth` ou até não restarem nós com `min_n` observações por folha. São esses dois limites que controlam o sobreajuste.",
          verificar = c("ml/tune", "ml/evaluate"),
          se_falhar = "Escolha `max_depth` e `min_n` no `ml/tune` (validação cruzada no treino) em vez de fixá-los a olho."),
        P("Uma árvore única é **instável**: pequenas mudanças nos dados (outra semente no `ml/split`) podem trocar os cortes e as regras.",
          se_falhar = "Refaça com outra semente e compare as regras; para previsão estável, `ml/forest`."),
        imp_arvore("a redução de impureza somada nos cortes (inclusive substitutos) de cada preditor"))),
      referencias = list(cart84, L$islr, L$esl, strobl,
        I("rpart", "rpart", "`method = \"anova\"` na regressão e `\"class\"` (Gini) na classificação; `rpart.control(maxdepth, minbucket = min_n, minsplit = 2·min_n, cp = 0)` — sem poda por complexidade."))),

    "ml/figs" = list(
      pressupostos = c(base, list(num,
        P("Na classificação, **duas classes**: a soma das árvores estima a probabilidade da segunda.",
          verificar = "data/group_summarise",
          se_falhar = "Com três ou mais classes, `ml/cart`, `ml/forest` ou `ml/xgboost`."),
        P("O **orçamento de divisões** (`max_splits`) controla a complexidade de todo o modelo; ele não é escolhido pelo bloco.",
          verificar = c("ml/tune", "ml/evaluate"),
          se_falhar = "Escolha `max_splits` e `min_n` no `ml/tune`."),
        imp_arvore("a redução de erro somada por preditor em todas as árvores"))),
      referencias = list(
        R(autores = c("Tan, Y. S.", "Singh, C.", "Nasseri, K.", "Agarwal, A.", "Duncan, J.", "Ronen, O.",
                      "Epland, M.", "Kornblith, A.", "Yu, B."), ano = 2025,
          titulo = "Fast Interpretable Greedy-Tree Sums",
          fonte = "Proceedings of the National Academy of Sciences, 122(7), e2310151122",
          doi = "10.1073/pnas.2310151122"),
        I("figsr", "figs", "`mode = \"regression\"` ou `\"classification\"` (binária), com `max_splits` e `min_n`."))),

    "ml/forest" = list(
      pressupostos = c(base, list(num,
        P("As árvores são **muitas o bastante** para a média estabilizar; o `mtry` (preditores sorteados por corte) e o `min_n` regulam a correlação entre elas e o sobreajuste.",
          verificar = c("ml/tune", "ml/evaluate"),
          se_falhar = "Escolha `mtry`, `min_n` e `max_depth` no `ml/tune`; mais árvores só custam tempo."),
        imp_arvore("a redução de impureza (Gini na classificação, variância na regressão) somada nas árvores"),
        C$semente)),
      referencias = list(breiman01, L$islr, L$esl, strobl,
        R(autores = c("Wright, M. N.", "Ziegler, A."), ano = 2017,
          titulo = "ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R",
          fonte = "Journal of Statistical Software, 77(1), 1-17", doi = "10.18637/jss.v077.i01",
          papel = "complementar"),
        I("ranger", "ranger", "`num.trees = trees`, `mtry` (0 = ⌊√p⌋), `min.node.size = min_n`, `max.depth`, `importance = \"impurity\"`; na classificação, floresta de probabilidade (`probability = TRUE`)."))),

    "ml/svm" = list(
      pressupostos = c(base, list(num,
        P("Os preditores estão em **escalas comparáveis**: o bloco padroniza cada um com média e desvio **do treino** e reaplica essa padronização na previsão, então isso já está garantido.",
          se_falhar = "Nada a fazer; não padronize antes com estatísticas do conjunto inteiro (vazaria o teste)."),
        P("`cost` e `gamma` (e o kernel) são **hiperparâmetros**: os valores padrão raramente são os bons, e o resultado é sensível a eles.",
          verificar = c("ml/tune", "ml/evaluate"),
          se_falhar = "Escolha-os no `ml/tune`, na escala logarítmica, nunca olhando o teste."),
        P("As probabilidades `.prob_*` saem de uma calibração à parte (Platt, com validação cruzada interna) e **podem discordar** da classe em `.pred`, que vem da margem.",
          verificar = "ml/confusion",
          se_falhar = "Use `.pred` para a matriz de confusão e as `.prob_*` só para a curva no `ml/roc`."),
        C$semente)),
      referencias = list(
        R(autores = c("Cortes, C.", "Vapnik, V."), ano = 1995, titulo = "Support-vector networks",
          fonte = "Machine Learning, 20(3), 273-297", doi = "10.1007/BF00994018"),
        L$islr, L$esl,
        R(autores = c("Chang, C.-C.", "Lin, C.-J."), ano = 2011, titulo = "LIBSVM: A library for support vector machines",
          fonte = "ACM Transactions on Intelligent Systems and Technology, 2(3), 1-27",
          doi = "10.1145/1961189.1961199", papel = "complementar"),
        I("e1071", "svm", "`type = \"eps-regression\"` ou `\"C-classification\"`, `kernel`, `cost`, `gamma`; `scale = TRUE` (padrão: centra e escala pelo treino); `probability = TRUE` na classificação."))),

    "ml/xgboost" = list(
      pressupostos = c(base, list(num,
        P("O **número de rodadas** (`nrounds`), a profundidade e a taxa de aprendizado (`eta`) controlam o sobreajuste. O bloco não faz parada antecipada: usa exatamente `nrounds`.",
          verificar = c("ml/tune", "ml/evaluate"),
          se_falhar = "Escolha os três juntos no `ml/tune` (eta menor pede mais rodadas); nunca pare pelo desempenho no teste."),
        imp_arvore("o ganho (Gain) somado nos cortes de cada preditor"))),
      referencias = list(
        R(autores = c("Chen, T.", "Guestrin, C."), ano = 2016, titulo = "XGBoost: A Scalable Tree Boosting System",
          fonte = "Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining, 785-794",
          doi = "10.1145/2939672.2939785"),
        L$esl, L$islr,
        I("xgboost", "xgb.train", "`objective` = `reg:squarederror`, `binary:logistic` ou `multi:softprob`; `max_depth`, `eta`, `nrounds`, `nthread = 1`; sem subamostragem de linhas nem de colunas (determinístico)."))))
}
