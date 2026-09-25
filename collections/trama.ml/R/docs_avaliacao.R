# Pressupostos e referências: escolha de hiperparâmetros e avaliação.

.tr_ml_docs_avaliacao <- function() {
  P <- .tr_ml_P; I <- .tr_ml_impl; R <- trama::tr_ref
  L <- .tr_ml_livros(); C <- .tr_ml_comuns()
  teste_fora <- P("As linhas avaliadas são do **teste**, que não participou do ajuste nem da escolha de hiperparâmetros. O bloco mede o que receber: no treino, ou num teste já usado para decidir, a medida sai otimista.",
    se_falhar = "Avalie a saída teste do `ml/split` passada pelo `ml/predict`; se o teste já foi usado para escolher, separe um novo teste ou reporte a estimativa como otimista.")
  desequilibrio <- P("Com **classes desequilibradas**, a acurácia engana: prever sempre a classe maioritária já a deixa alta. A acurácia balanceada e o macro F1 dão peso igual a cada classe.",
    verificar = "data/group_summarise",
    se_falhar = "Conte as linhas por classe no `data/group_summarise` e leia a `balanced_accuracy` ou o `macro_f1` e a `ml/confusion`, não só a `accuracy`.")
  fawcett <- R(autores = "Fawcett, T.", ano = 2006, titulo = "An introduction to ROC analysis",
               fonte = "Pattern Recognition Letters, 27(8), 861-874", doi = "10.1016/j.patrec.2005.10.010")
  brodersen <- R(autores = c("Brodersen, K. H.", "Ong, C. S.", "Stephan, K. E.", "Buhmann, J. M."), ano = 2010,
                 titulo = "The Balanced Accuracy and Its Posterior Distribution",
                 fonte = "20th International Conference on Pattern Recognition (ICPR), 3121-3124",
                 doi = "10.1109/ICPR.2010.764", papel = "complementar")
  list(
    "ml/tune" = list(
      pressupostos = list(
        P("Entra **só o treino**: os folds saem das linhas recebidas e o vencedor é reajustado nelas todas. Se o teste entrar aqui, ele deixa de ser teste.",
          se_falhar = "Ligue a saída treino do `ml/split`; a teste vai só ao `ml/predict`."),
        C$sem_vazamento,
        P("Os folds são **aleatórios** (estratificados pela classe na classificação): supõem linhas independentes, sem ordem no tempo nem grupos repartidos entre folds.",
          se_falhar = "Validação cruzada temporal ou por grupo ainda sem bloco no trama; com dependência, reduza o peso dado à média dos folds e confira no teste."),
        P("A **média dos folds do vencedor é otimista**: foi a melhor entre muitas tentativas, e parte da vantagem é sorte. Ela serve para escolher, não para reportar o desempenho.",
          verificar = "ml/tuning_plot",
          se_falhar = "Reporte o desempenho medido no teste com `ml/predict` e `ml/evaluate`. Validação cruzada aninhada ainda sem bloco no trama."),
        P("A **métrica** escolhida é a que importa no problema; `auto` usa RMSE (regressão) e macro F1 (classificação).",
          verificar = "data/group_summarise",
          se_falhar = "Com classes desequilibradas, prefira `balanced_accuracy` ou `macro_f1` a `accuracy`."),
        P("As **tentativas bastam** para o espaço de busca (busca aleatória): se o melhor ainda melhora perto do fim, o orçamento foi curto.",
          verificar = "ml/tuning_plot",
          se_falhar = "Aumente `tentativas` ou mude a `amplitude`."),
        C$semente),
      referencias = list(
        R(autores = "Stone, M.", ano = 1974, titulo = "Cross-Validatory Choice and Assessment of Statistical Predictions",
          fonte = "Journal of the Royal Statistical Society. Series B, 36(2), 111-133",
          doi = "10.1111/j.2517-6161.1974.tb00994.x"),
        R(autores = c("Bergstra, J.", "Bengio, Y."), ano = 2012, titulo = "Random Search for Hyper-Parameter Optimization",
          fonte = "Journal of Machine Learning Research, 13, 281-305",
          url = "https://www.jmlr.org/papers/v13/bergstra12a.html"),
        L$islr, L$kuhn,
        R(autores = c("Varma, S.", "Simon, R."), ano = 2006,
          titulo = "Bias in error estimation when using cross-validation for model selection",
          fonte = "BMC Bioinformatics, 7, 91", doi = "10.1186/1471-2105-7-91", papel = "complementar"),
        I("trama.ml", "tr_ml_tune", "Implementação própria: busca aleatória (log-uniforme para `cost`, `gamma` e `eta`) avaliada nos mesmos k folds; o vencedor, pela média, é reajustado com `tr_ml_fit` em todas as linhas."))),

    "ml/evaluate" = list(
      pressupostos = list(teste_fora, desequilibrio,
        P("Na regressão, o **R²** compara com a média do próprio conjunto avaliado; no teste pode ser negativo (pior que prever a média) e é indefinido com resposta constante.",
          verificar = "ml/residuals",
          se_falhar = "Leia RMSE e MAE, na unidade da resposta, junto do gráfico de resíduos."),
        P("A medida vem de **um único teste**: com poucas linhas ela varia muito de uma divisão para outra.",
          se_falhar = "Refaça o `ml/split` com outras sementes e compare; ou use o `ml/tune`, que mostra a variação entre folds.")),
      referencias = list(L$islr, L$kuhn, brodersen,
        I("trama.ml", "tr_ml_evaluate", "Cálculo próprio: MAE, RMSE e R² = 1 − SQE/SQT; acurácia, acurácia balanceada (média dos recalls) e macro F1 sobre as classes observadas no alvo. Nenhuma linha é descartada."))),

    "ml/confusion" = list(
      pressupostos = list(teste_fora, desequilibrio,
        P("Observado e previsto usam os **mesmos rótulos** de classe; um rótulo escrito diferente vira uma classe a mais.",
          se_falhar = "Padronize os rótulos com o `data/mutate` antes.")),
      referencias = list(fawcett, L$islr, L$kuhn,
        I("trama.ml", "tr_ml_confusion", "Cálculo próprio com `base::table`, sobre a união das classes observadas e previstas, incluindo as contagens zero."))),

    "ml/roc" = list(
      pressupostos = list(teste_fora,
        P("**Duas classes**, e a coluna de probabilidade é a **da classe positiva**. Com `positiva` vazia o bloco usa a segunda classe na ordem em que aparece nas linhas, não o nome da coluna: se não baterem, a curva sai espelhada (AUC = 1 − AUC).",
          se_falhar = "Preencha `positiva` com a classe da coluna `.prob_<classe>` escolhida."),
        P("A AUC resume **todos os cortes**, inclusive os que ninguém usaria, e não muda com o desequilíbrio das classes — por isso mesmo pode parecer boa quando a classe rara é mal prevista.",
          verificar = c("ml/confusion", "data/group_summarise"),
          se_falhar = "Leia junto a matriz de confusão e a acurácia balanceada no `ml/evaluate`. Curva precisão-revocação ainda sem bloco no trama.")),
      referencias = list(
        R(autores = c("Hanley, J. A.", "McNeil, B. J."), ano = 1982,
          titulo = "The meaning and use of the area under a receiver operating characteristic (ROC) curve",
          fonte = "Radiology, 143(1), 29-36", doi = "10.1148/radiology.143.1.7063747"),
        fawcett, L$islr,
        R(autores = c("Saito, T.", "Rehmsmeier, M."), ano = 2015,
          titulo = "The Precision-Recall Plot Is More Informative than the ROC Plot When Evaluating Binary Classifiers on Imbalanced Datasets",
          fonte = "PLOS ONE, 10(3), e0118432", doi = "10.1371/journal.pone.0118432", papel = "complementar"),
        I("trama.ml", "tr_ml_roc", "Cálculo próprio: ordena pela probabilidade, agrupa empates num só degrau e integra a AUC pela regra do trapézio.")))
  )
}
