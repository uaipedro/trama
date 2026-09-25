# Pressupostos e referências: prever e medir a previsão (confusão, ROC,
# métricas).
#
# Estes blocos vieram da `ml` e da `multi` na Fase 4 (contrato de modelo); os
# textos juntam o que as duas coleções já tinham conferido para os blocos
# antigos (`ml/evaluate`, `ml/confusion`, `ml/roc`, `multi/confusion`,
# `multi/roc`), reescritos para os dois caminhos daqui: só `modelo` (validação
# cruzada no treino) ou `modelo` + `dados` / tabela pronta (um teste).

.tr_models_docs_avaliar <- function() {
  P <- .tr_models_P; I <- .tr_models_impl; R <- trama::tr_ref
  fora_amostra <- P("A medida que interessa é a **fora da amostra**: com o teste separado (`ml/split`) em `dados`, ou pela validação `cruzada` quando só o modelo está ligado. A resubstituição sai otimista, e mais ainda com muitos preditores e poucos casos.",
    se_falhar = "Ligue a saída teste do `ml/split` em `dados`, ou deixe a `validacao` em `cruzada`; se o teste já foi usado para escolher, reporte a medida como otimista.")
  desequilibrio <- P("Com **classes desequilibradas**, a acurácia engana: prever sempre a classe maioritária já a deixa alta. A acurácia balanceada e o macro F1 dão peso igual a cada classe.",
    verificar = "data/group_summarise",
    se_falhar = "Conte as linhas por classe no `data/group_summarise` e leia a `balanced_accuracy` ou o `macro_f1` e a `models/confusion`, não só a `accuracy`.")
  fawcett <- R(autores = "Fawcett, T.", ano = 2006, titulo = "An introduction to ROC analysis",
               fonte = "Pattern Recognition Letters, 27(8), 861-874", doi = "10.1016/j.patrec.2005.10.010")
  hanley <- R(autores = c("Hanley, J. A.", "McNeil, B. J."), ano = 1982,
              titulo = "The meaning and use of the area under a receiver operating characteristic (ROC) curve",
              fonte = "Radiology, 143(1), 29-36", doi = "10.1148/radiology.143.1.7063747")
  saito <- R(autores = c("Saito, T.", "Rehmsmeier, M."), ano = 2015,
             titulo = "The Precision-Recall Plot Is More Informative than the ROC Plot When Evaluating Binary Classifiers on Imbalanced Datasets",
             fonte = "PLOS ONE, 10(3), e0118432", doi = "10.1371/journal.pone.0118432", papel = "complementar")
  lachenbruch <- R(autores = c("Lachenbruch, P. A.", "Mickey, M. R."), ano = 1968,
                   titulo = "Estimation of error rates in discriminant analysis",
                   fonte = "Technometrics, 10(1), 1-11", doi = "10.1080/00401706.1968.10490530")
  islr <- R(autores = c("James, G.", "Witten, D.", "Hastie, T.", "Tibshirani, R."), ano = 2021,
            titulo = "An Introduction to Statistical Learning: with Applications in R",
            fonte = "2. ed. New York: Springer", doi = "10.1007/978-1-0716-1418-1",
            papel = "livro-texto")
  kuhn <- R(autores = c("Kuhn, M.", "Johnson, K."), ano = 2013,
            titulo = "Applied Predictive Modeling", fonte = "New York: Springer",
            doi = "10.1007/978-1-4614-6849-3", papel = "livro-texto")
  list(
    "models/evaluate" = list(
      pressupostos = list(fora_amostra, desequilibrio,
        P("Na regressão, o **R²** compara com a média do próprio conjunto avaliado; no teste pode ser negativo (pior que prever a média) e é indefinido com resposta constante.",
          verificar = "ml/residuals",
          se_falhar = "Leia RMSE e MAE, na unidade da resposta, junto do gráfico de resíduos."),
        P("O **kappa** desconta a concordância esperada pelas marginais, mas depende da prevalência: com uma classe muito rara ele cai mesmo com boa acurácia, e não há escala universal do que é “bom”.",
          se_falhar = "Leia o kappa junto da acurácia balanceada e das métricas por classe."),
        P("Precisão, revocação e F1 **por classe** tratam cada classe contra as demais; com poucas linhas de uma classe (coluna `n`), os valores dela variam muito. Uma classe nunca prevista tem precisão 0 por convenção.",
          verificar = "models/confusion",
          se_falhar = "Confira as contagens na `models/confusion` antes de interpretar a classe.")),
      referencias = list(islr, kuhn,
        R(autores = c("Brodersen, K. H.", "Ong, C. S.", "Stephan, K. E.", "Buhmann, J. M."), ano = 2010,
          titulo = "The Balanced Accuracy and Its Posterior Distribution",
          fonte = "20th International Conference on Pattern Recognition (ICPR), 3121-3124",
          doi = "10.1109/ICPR.2010.764", papel = "complementar"),
        R(autores = "Cohen, J.", ano = 1960, titulo = "A Coefficient of Agreement for Nominal Scales",
          fonte = "Educational and Psychological Measurement, 20(1), 37-46", doi = "10.1177/001316446002000104"),
        R(autores = c("Sokolova, M.", "Lapalme, G."), ano = 2009,
          titulo = "A systematic analysis of performance measures for classification tasks",
          fonte = "Information Processing & Management, 45(4), 427-437", doi = "10.1016/j.ipm.2009.03.002",
          papel = "complementar"),
        I("trama.models", "tr_models_evaluate", "Cálculo próprio: MAE, RMSE e R² = 1 − SQE/SQT; na classificação, por classe (um contra todos) precisão, revocação e F1, com médias macro sobre as classes observadas e ponderadas pelo suporte; acurácia balanceada = média das revocações; kappa = (po − pe)/(1 − pe) com pe das marginais; na binária, sensibilidade e especificidade da positiva."))),

    "models/confusion" = list(
      pressupostos = list(fora_amostra, desequilibrio,
        P("Real e previsto usam os **mesmos rótulos** de classe; um rótulo escrito diferente vira uma classe a mais.",
          se_falhar = "Padronize os rótulos com o `data/mutate` antes.")),
      referencias = list(fawcett, lachenbruch, islr,
        I("trama.models", "tr_models_confusion", "Cálculo próprio com `base::table` sobre os níveis da resposta, incluindo as contagens zero; sem `dados`, as previsões são as da validação cruzada do contrato (`tr_models_predict_cv`)."))),

    "models/roc" = list(
      pressupostos = list(fora_amostra,
        P("A probabilidade é a **da classe positiva**. Com `positiva` vazia, na binária ela é o segundo nível; no modo tabela, com a coluna `probabilidade` informada, a classe é deduzida do nome `prob_<classe>`, e o bloco pede `positiva` se o nome não indicar uma classe observada. Classe e coluna trocadas espelham a curva (AUC = 1 − AUC).",
          se_falhar = "Com uma coluna de nome livre, preencha `positiva` com a classe cuja probabilidade ela contém."),
        P("A AUC resume **todos os cortes**, inclusive os que ninguém usaria, e não muda com o desequilíbrio das classes — por isso mesmo pode parecer boa quando a classe rara é mal prevista.",
          verificar = c("models/confusion", "data/group_summarise"),
          se_falhar = "Leia junto a matriz de confusão, a acurácia balanceada no `models/evaluate` e, com classe rara, a `ml/pr_curve`."),
        P("Com três ou mais classes, cada curva é **uma classe contra as outras**: a AUC de cada uma não soma nem resume o classificador inteiro.")),
      referencias = list(hanley, fawcett, saito,
        I("trama.models", "tr_models_roc", "Cálculo próprio: um ponto por escore distinto (empates andam na diagonal) e AUC de Mann-Whitney com meio ponto por empate, igual à área trapezoidal da curva.")))
  )
}
