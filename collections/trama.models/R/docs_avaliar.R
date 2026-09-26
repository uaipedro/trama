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
  fora_amostra <- P("A medida que interessa é a **fora da amostra**: com o teste separado (`ml/split`) em `dados`, ou pela validação `cruzada` quando só o modelo está ligado. A resubstituição sai otimista, e mais ainda com muitos preditores e poucos casos. Previsões do treino marcado pelo `ml/split` são recusadas, salvo `permitir_treino` (que acrescenta a nota de otimismo); tabelas sem essa marca (divisão feita por fora, treino e teste juntados) são medidas como chegam, e num teste já usado para decidir a medida também sai otimista — isso nenhuma marca detecta.",
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
        P("Precisão, revocação e F1 **por classe** tratam cada classe contra as demais; com poucas linhas de uma classe (coluna `n`), os valores dela variam muito. Uma classe nunca prevista tem precisão **indefinida** (0/0): sai NA e fica fora das médias macro e ponderada, como `zero_division = np.nan` do scikit-learn — o padrão do scikit-learn põe 0 e puxa a média para baixo, então compare com cuidado.",
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
        R(autores = "Cohen, J.", ano = 1960, titulo = "A coefficient of agreement for nominal scales",
          fonte = "Educational and Psychological Measurement, 20(1), 37-46", doi = "10.1177/001316446002000104"),
        R(autores = c("Brodersen, K. H.", "Ong, C. S.", "Stephan, K. E.", "Buhmann, J. M."), ano = 2010,
          titulo = "The balanced accuracy and its posterior distribution",
          fonte = "20th International Conference on Pattern Recognition, 3121-3124", doi = "10.1109/ICPR.2010.764",
          papel = "complementar"),
        R(autores = c("Sokolova, M.", "Lapalme, G."), ano = 2009,
          titulo = "A systematic analysis of performance measures for classification tasks",
          fonte = "Information Processing & Management, 45(4), 427-437", doi = "10.1016/j.ipm.2009.03.002",
          papel = "complementar"),
        I("trama.models", "tr_models_confusion", "Cálculo próprio com `base::table` sobre os níveis da resposta, incluindo as contagens zero; sem `dados`, as previsões são as da validação cruzada do contrato (`tr_models_predict_cv`). Métricas (`tabela = \"métricas\"`) por implementação própria (porte da `multi/confusion` da main); kappa conferido contra `irr::kappa2` e `psych::cohen.kappa`."))),

    "models/roc" = list(
      pressupostos = list(fora_amostra,
        P("A probabilidade é a **da classe positiva**. Com `positiva` vazia, na binária ela é o segundo nível; no modo tabela, com a coluna `probabilidade` informada, a classe é deduzida do nome `prob_<classe>`, e o bloco pede `positiva` se o nome não indicar uma classe observada. Classe e coluna trocadas espelham a curva (AUC = 1 − AUC).",
          se_falhar = "Com uma coluna de nome livre, preencha `positiva` com a classe cuja probabilidade ela contém."),
        P("A AUC resume **todos os cortes**, inclusive os que ninguém usaria, e não muda com o desequilíbrio das classes — por isso mesmo pode parecer boa quando a classe rara é mal prevista.",
          verificar = c("models/confusion", "data/group_summarise"),
          se_falhar = "Leia junto a matriz de confusão, a acurácia balanceada no `models/evaluate` e, com classe rara, a `models/pr_curve`."),
        P("Com três ou mais classes, cada curva é **uma classe contra as outras**: a AUC de cada uma não soma nem resume o classificador inteiro. O resumo é a AUC multiclasse M de Hand & Till (2001), no subtítulo (sem `positiva`): média, sobre os pares de classes, da AUC do par; não depende das proporções das classes, mas pondera todos os pares igualmente e não tem intervalo.",
          se_falhar = "Para saber qual par se confunde, leia a matriz de confusão (`models/confusion`)."),
        P("O **intervalo de DeLong** é assintótico (normal): com poucos positivos ou negativos, ou AUC perto de 1, a cobertura fica abaixo do nominal; o bloco corta os limites em [0, 1]. Com AUC = 0 ou 1 (variância zero) ou menos de duas linhas numa classe, o IC sai **indisponível** (NA), com a nota em `auc_nota` e na legenda; a curva e a AUC continuam.",
          se_falhar = "Leia o intervalo como aproximado e a largura como sinal de pouca informação; aumente a amostra da classe rara ou repita a divisão com outras sementes."),
        P("Com validação cruzada, as probabilidades vêm de n ajustes, e o intervalo as trata como **um escore fixo**: a incerteza de ter estimado o modelo não entra."),
        P("O **corte de Youden** é escolhido nas mesmas linhas em que é lido: a sensibilidade e a especificidade nele são otimistas, e J pesa igualmente falso positivo e falso negativo, o que raramente reflete os custos reais.",
          se_falhar = "Escolha o corte num conjunto de validação (ou pelos custos do problema) e leia o desempenho dele no teste, com o `models/confusion`.")),
      referencias = list(hanley, fawcett, saito,
        R(autores = c("DeLong, E. R.", "DeLong, D. M.", "Clarke-Pearson, D. L."), ano = 1988,
          titulo = "Comparing the areas under two or more correlated receiver operating characteristic curves: a nonparametric approach",
          fonte = "Biometrics, 44(3), 837-845", doi = "10.2307/2531595"),
        R(autores = c("Hand, D. J.", "Till, R. J."), ano = 2001,
          titulo = "A simple generalisation of the area under the ROC curve for multiple class classification problems",
          fonte = "Machine Learning, 45(2), 171-186", doi = "10.1023/A:1010920819831"),
        R(autores = "Youden, W. J.", ano = 1950, titulo = "Index for rating diagnostic tests",
          fonte = "Cancer, 3(1), 32-35", doi = "10.1002/1097-0142(1950)3:1<32::AID-CNCR2820030106>3.0.CO;2-3"),
        R(autores = c("Robin, X.", "Turck, N.", "Hainard, A.", "Tiberti, N.", "Lisacek, F.", "Sanchez, J.-C.", "Müller, M."),
          ano = 2011, titulo = "pROC: an open-source package for R and S+ to analyze and compare ROC curves",
          fonte = "BMC Bioinformatics, 12, 77", doi = "10.1186/1471-2105-12-77", papel = "complementar"),
        I("trama.models", "tr_models_roc", "Cálculo próprio: um ponto por escore distinto (empates andam na diagonal) e AUC de Mann-Whitney com meio ponto por empate, igual à área trapezoidal da curva; IC de DeLong pelas componentes estruturais (normal, cortado em [0, 1]), conferido contra `pROC::ci.auc(method = \"delong\")` a 1e-8; corte de Youden = máximo de sensibilidade + especificidade − 1 (empate: maior limiar), conferido contra `pROC::coords(best.method = \"youden\")`; M de Hand & Till conferido contra `pROC::multiclass.roc` a 1e-10. Portes da `ml/roc` e da `multi/roc` da main."))),

    "models/pr_curve" = list(
      pressupostos = list(fora_amostra,
        P("**A probabilidade é a **da classe positiva** — a classe de interesse, em geral a rara; com três ou mais classes, ela contra as outras. Com `positiva` vazia: no modo tabela a classe vem do nome da coluna (`prob_<classe>`), e com coluna de nome livre o bloco pede `positiva`; com modelo, é o segundo nível.",
          se_falhar = "Preencha `positiva` com a classe cuja probabilidade a coluna contém."),
        P("A referência do acaso é a **prevalência** da classe positiva, não 0,5: a AP só diz algo comparada a ela, e a curva de um conjunto com outra prevalência não é comparável (Saito & Rehmsmeier 2015).",
          verificar = "data/group_summarise",
          se_falhar = "Compare modelos no mesmo teste; para comparar populações diferentes, leia a ROC."),
        P("A **AP** (soma de ganho de revocação × precisão) e a **área interpolada** de Davis & Goadrich são estimativas diferentes da mesma área; ligar os pontos por reta superestima a área e não é usado (Davis & Goadrich 2006).",
          se_falhar = "Reporte uma delas nomeada; a AP é a do `yardstick` e do scikit-learn, a área é a do `PRROC`."),
        P("Com **poucos positivos** a curva tem poucos degraus e a AP varia muito de um teste para outro.",
          se_falhar = "Leia a quantidade de positivos (`prevalencia` × n) e repita a divisão com outras sementes.")),
      referencias = list(
        R(autores = c("Saito, T.", "Rehmsmeier, M."), ano = 2015,
          titulo = "The Precision-Recall Plot Is More Informative than the ROC Plot When Evaluating Binary Classifiers on Imbalanced Datasets",
          fonte = "PLOS ONE, 10(3), e0118432", doi = "10.1371/journal.pone.0118432"),
        R(autores = c("Davis, J.", "Goadrich, M."), ano = 2006,
          titulo = "The relationship between Precision-Recall and ROC curves",
          fonte = "Proceedings of the 23rd International Conference on Machine Learning (ICML), 233-240",
          doi = "10.1145/1143844.1143874"),
        R(autores = c("Keilwagen, J.", "Grosse, I.", "Grau, J."), ano = 2014,
          titulo = "Area under Precision-Recall Curves for Weighted and Unweighted Data",
          fonte = "PLoS ONE, 9(3), e92209", doi = "10.1371/journal.pone.0092209", papel = "complementar"),
        R(autores = c("Su, W.", "Yuan, Y.", "Zhu, M."), ano = 2015,
          titulo = "A Relationship between the Average Precision and the Area Under the ROC Curve",
          fonte = "Proceedings of the 2015 International Conference on the Theory of Information Retrieval (ICTIR), 349-352",
          doi = "10.1145/2808194.2809481", papel = "complementar"),
        I("trama.models", "tr_models_pr_curve", "Cálculo próprio (porte da `ml/pr_curve` da main): um ponto por limiar distinto (empates num degrau); AP = Σ ΔR·P; área com a interpolação de Davis & Goadrich integrada em forma fechada. Conferido contra `yardstick::average_precision` e `PRROC::pr.curve` (`auc.integral`)."))),
    "models/predict" = list(
      pressupostos = list(
        P("As linhas novas estão **dentro da faixa** dos preditores do ajuste: fora dela é extrapolação, e a previsão (e o intervalo) supõe que a forma do modelo continua valendo onde não houve dado. Na regressão múltipla a faixa é a da nuvem conjunta, não a de cada coluna.",
          verificar = c("view/points", "models/residuals"),
          se_falhar = "Leia a previsão fora da faixa como hipótese, não como estimativa; o intervalo não cobre o erro de forma do modelo."),
        P("O modelo está **bem especificado** e os pressupostos dele valem (os do bloco que o ajustou): a previsão herda a forma, e o intervalo herda a normalidade e a variância constante do erro.",
          verificar = c("models/plot_diagnostics", "models/shapiro_residuals")),
        P("O intervalo certo para a pergunta: **confiança** cobre a MÉDIA da resposta naquele x; **predição** cobre UMA observação nova, e é mais largo porque soma a variância do erro. Para dizer onde cairá a próxima parcela, é o de predição.",
          se_falhar = "Troque o param Intervalo."),
        P("A unidade nova vem da **mesma população** do ajuste (mesmas condições, mesmos níveis de fator; o bloco recusa nível que o ajuste não viu)."),
        P("A medida de acerto no próprio treino por **resubstituição** sai otimista.",
          verificar = "models/evaluate",
          se_falhar = "Use a validação `cruzada`, ou dados de teste separados.")),
      referencias = list(
        R(autores = c("Draper, N. R.", "Smith, H."), ano = 1998, titulo = "Applied Regression Analysis",
          fonte = "3. ed. New York: Wiley", doi = "10.1002/9781118625590", papel = "livro-texto"),
        .tr_models_livros()$rencher,
        I("stats", "predict", "Pelo contrato de cada modelo (`predict.lm`, `predict.glm` com `type = \"response\"`, `predict.merMod`, `predict.nls`); intervalo só no `lm` (`interval = \"confidence\"`/`\"prediction\"`), conferido contra a fórmula ŷ0 ± t·s·√(h0) e √(1 + h0) com h0 = x0'(X'X)⁻¹x0 feita à mão (1e-10).")))
  )
}
