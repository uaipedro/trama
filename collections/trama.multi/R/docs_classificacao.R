# Pressupostos e referências: discriminante, M de Box, logística e a avaliação
# dos classificadores (matriz de confusão e ROC).

.tr_multi_docs_classificacao <- function() {
  P <- .tr_multi_P; I <- .tr_multi_impl; R <- trama::tr_ref
  L <- .tr_multi_livros()
  fisher <- R(autores = "Fisher, R. A.", ano = 1936, titulo = "The use of multiple measurements in taxonomic problems",
              fonte = "Annals of Eugenics, 7(2), 179-188", doi = "10.1111/j.1469-1809.1936.tb02137.x")
  box <- R(autores = "Box, G. E. P.", ano = 1949, titulo = "A general distribution theory for a class of likelihood criteria",
           fonte = "Biometrika, 36(3-4), 317-346", doi = "10.1093/biomet/36.3-4.317")
  lachenbruch <- R(autores = c("Lachenbruch, P. A.", "Mickey, M. R."), ano = 1968,
                   titulo = "Estimation of error rates in discriminant analysis",
                   fonte = "Technometrics, 10(1), 1-11", doi = "10.1080/00401706.1968.10490530")
  hanley <- R(autores = c("Hanley, J. A.", "McNeil, B. J."), ano = 1982,
              titulo = "The meaning and use of the area under a receiver operating characteristic (ROC) curve",
              fonte = "Radiology, 143(1), 29-36", doi = "10.1148/radiology.143.1.7063747")
  fawcett <- R(autores = "Fawcett, T.", ano = 2006, titulo = "An introduction to ROC analysis",
               fonte = "Pattern Recognition Letters, 27(8), 861-874", doi = "10.1016/j.patrec.2005.10.010")
  saito <- R(autores = c("Saito, T.", "Rehmsmeier, M."), ano = 2015,
             titulo = "The precision-recall plot is more informative than the ROC plot when evaluating binary classifiers on imbalanced datasets",
             fonte = "PLOS ONE, 10(3), e0118432", doi = "10.1371/journal.pone.0118432", papel = "complementar")
  indep <- P("As observações são **independentes** (um indivíduo por linha, sem medidas repetidas).")
  mvn <- P("Dentro de cada grupo, os preditores são **normais multivariados**.",
           verificar = c("multi/mardia", "models/shapiro", "view/qq"),
           se_falhar = "Teste no `multi/mardia` com o grupo informado (ele testa dentro de cada grupo). Com preditores assimétricos, contagens ou 0/1, use a `multi/logistic`.")
  fora_amostra <- P("A taxa de acerto que interessa é a **fora da amostra**: por resubstituição ela sai otimista, e mais ainda com muitos preditores e poucos casos.",
                    verificar = "multi/confusion",
                    se_falhar = "Leia a validação `cruzada` (deixa-um-fora), o padrão do bloco.")
  desbalanceio <- P("Com **grupos desbalanceados**, a taxa de acerto geral engana: prever sempre o grupo maior já acerta a proporção dele. Leia a taxa de cada grupo (a sensibilidade de cada um).",
                    verificar = "data/group_summarise",
                    se_falhar = "Compare pela AUC na `multi/roc`; na discriminante, experimente `priors = \"iguais\"`. O trama ainda não tem acurácia balanceada nem curva precisão-revocação (lacuna registrada).")
  mardia70 <- R(autores = "Mardia, K. V.", ano = 1970,
                titulo = "Measures of multivariate skewness and kurtosis with applications",
                fonte = "Biometrika, 57(3), 519-530", doi = "10.1093/biomet/57.3.519")
  mardia74 <- R(autores = "Mardia, K. V.", ano = 1974,
                titulo = "Applications of some measures of multivariate skewness and kurtosis in testing normality and robustness studies",
                fonte = "Sankhyā, Series B, 36(2), 115-128", papel = "complementar")
  list(
    "multi/mardia" = list(
      pressupostos = list(indep,
        P("As estatísticas têm distribuição **assintótica** (χ² na assimetria, normal na curtose): com n pequeno a curtose é conservadora e a assimetria liberal — leia a linha de **amostra pequena** (Mardia 1974) quando n < 20.",
          se_falhar = "Com poucos casos, olhe também os gráficos de cada variável (`view/qq`) e não trate o não rejeitar como prova de normalidade."),
        P("Os dados são de **uma** população: grupos com médias diferentes misturados na mesma tabela formam uma mistura que não é normal.",
          se_falhar = "Informe o **grupo** para testar dentro de cada um, que é o que a discriminante e o M de Box supõem."),
        P("A covariância de cada grupo é **inversível**: n ≥ p + 2 e nenhuma variável é combinação exata das outras (o bloco recusa senão).",
          verificar = "multi/correlation_matrix")),
      referencias = list(mardia70, mardia74, L$johnson, L$ferreira,
        I("trama.multi", "tr_multi_mardia", "Implementação própria com a covariância de divisor n (Mardia 1970; `MVN::mardia` com `use_population = TRUE`); a correção de amostra pequena usa o fator k de Mardia (1974), o mesmo de `MVN::mardia` e `psych::mardia`. Conferido contra `psych::mardia` reescalado (divisor n − 1)."))),

    "multi/discriminant" = list(
      pressupostos = list(indep, mvn,
        P("Na **linear**, as matrizes de covariância são **iguais** em todos os grupos; só as médias diferem.",
          verificar = "multi/box_m",
          se_falhar = "Ajuste a `quadrática` e compare as duas pela taxa cruzada no `multi/confusion` antes de trocar: o M de Box rejeita com facilidade."),
        P("Sem **colinearidade** exata entre os preditores dentro dos grupos (o bloco recusa senão), e **observações suficientes** por grupo — na quadrática, bem mais que p + 1.",
          verificar = "multi/correlation_matrix",
          se_falhar = "Tire o preditor redundante ou use a linear."),
        P("As **priors** refletem a população em que a regra vai ser usada: proporcionais só quando a tabela a representa.",
          se_falhar = "Com tamanhos de grupo fixados pela amostragem, use `priors = \"iguais\"`."),
        fora_amostra),
      referencias = list(fisher, L$johnson, L$mingoti, L$ferreira, L$mass,
        I("MASS", "lda", "`lda(x, grouping, prior)` na linear e `MASS::qda` na quadrática; a validação cruzada da classificação é o `CV = TRUE` delas. Wilks, correlações canônicas e o M de Box são implementação própria."))),

    "multi/box_m" = list(
      pressupostos = list(indep,
        P("Dentro de cada grupo, as variáveis são **normais multivariadas**: o M de Box é muito sensível a caudas pesadas, e rejeita por falta de normalidade tanto quanto por covariâncias diferentes.",
          verificar = c("multi/mardia", "view/qq"),
          se_falhar = "Com o `multi/mardia` rejeitando, não leia o M de Box como teste de covariâncias: decida entre linear e quadrática pela taxa cruzada no `multi/confusion`."),
        P("Cada grupo tem **observações suficientes** (pelo menos p + 1, e o bloco recusa senão) para a aproximação qui-quadrado; com amostra grande, o teste rejeita diferenças que não mudam a classificação.")),
      referencias = list(box, L$johnson, L$ferreira,
        I("trama.multi", "tr_multi_box_m", "Implementação própria: M = (N − g) ln|Sₚ| − Σ(nᵢ − 1) ln|Sᵢ| com a correção de Box para a qui-quadrado, p(p + 1)(g − 1)/2 gl."))),

    "multi/logistic" = list(
      pressupostos = list(indep,
        P("O **logito** (log da chance) é **linear** em cada preditor numérico.",
          verificar = c("models/glm", "models/compare"),
          se_falhar = "Na binária, ajuste um `models/glm` binomial com e sem `poly(x, 2)` do preditor suspeito no campo `formula` (ex.: `y ~ poly(x, 2) + z`) e compare no `models/compare`; se a curvatura importar, inclua a transformação num `data/mutate` antes."),
        P("**Sem separação** completa: se algum grupo é isolado sem sobreposição, os coeficientes vão ao infinito. O bloco detecta e avisa em `separacao`.",
          se_falhar = "Na binária, use `metodo = \"firth\"`: a verossimilhança penalizada dá coeficientes finitos e intervalos perfilados (Heinze & Schemper 2002). A classificação continua valendo nos dois métodos."),
        P("**Eventos suficientes por preditor**: com poucos casos no grupo menor (a regra usual é pelo menos 10 por preditor), coeficientes e erros de Wald ficam viesados.",
          verificar = c("data/group_summarise", "multi/jackknife_logistic"),
          se_falhar = "Use menos preditores; na binária, `metodo = \"firth\"` reduz o viés de ordem 1/n dos coeficientes (Firth 1993)."),
        P("Sem **colinearidade** exata entre os preditores (o bloco recusa senão).",
          verificar = "multi/correlation_matrix"),
        fora_amostra),
      referencias = list(
        R(autores = "Cox, D. R.", ano = 1958, titulo = "The regression analysis of binary sequences",
          fonte = "Journal of the Royal Statistical Society. Series B, 20(2), 215-232",
          doi = "10.1111/j.2517-6161.1958.tb00292.x"),
        R(autores = c("Albert, A.", "Anderson, J. A."), ano = 1984,
          titulo = "On the existence of maximum likelihood estimates in logistic regression models",
          fonte = "Biometrika, 71(1), 1-10", doi = "10.1093/biomet/71.1.1", papel = "complementar"),
        R(autores = c("Peduzzi, P.", "Concato, J.", "Kemper, E.", "Holford, T. R.", "Feinstein, A. R."), ano = 1996,
          titulo = "A simulation study of the number of events per variable in logistic regression analysis",
          fonte = "Journal of Clinical Epidemiology, 49(12), 1373-1379",
          doi = "10.1016/S0895-4356(96)00236-3", papel = "complementar"),
        R(autores = "Firth, D.", ano = 1993, titulo = "Bias reduction of maximum likelihood estimates",
          fonte = "Biometrika, 80(1), 27-38", doi = "10.1093/biomet/80.1.27"),
        R(autores = c("Heinze, G.", "Schemper, M."), ano = 2002,
          titulo = "A solution to the problem of separation in logistic regression",
          fonte = "Statistics in Medicine, 21(16), 2409-2419", doi = "10.1002/sim.1047"),
        L$hosmer, L$mass,
        I("stats", "glm", "Dois grupos: `glm(family = binomial())`. Três ou mais: `nnet::multinom(maxit = 1000)`. Separação detectada por regra própria sobre as probabilidades ajustadas."),
        I("trama.multi", "tr_multi_logistic", "`metodo = \"firth\"`: implementação própria (Newton na verossimilhança penalizada, intervalos perfilados por `uniroot`), conferida contra `logistf::logistf` no `sex2` (coeficientes a 1e-6, limites e p a 1e-4). EP pela inversa da informação de Fisher em β̂; o `logistf` usa (X'W(1 + h)X)⁻¹."))),

    "multi/confusion" = list(
      pressupostos = list(fora_amostra, desbalanceio,
        P("O classificador de entrada atende aos seus pressupostos (os da discriminante ou da logística); a matriz mede o acerto, não conserta o modelo. O `multi/box_m` só se aplica quando a entrada é um discriminante.",
          verificar = "multi/box_m")),
      referencias = list(lachenbruch, L$johnson, L$mass,
        I("MASS", "lda", "Deixa-um-fora da discriminante pelo `CV = TRUE` de `MASS::lda`/`qda`; na logística, n reajustes do `glm`/`nnet::multinom`, cada um sem uma linha."))),

    "multi/roc" = list(
      pressupostos = list(fora_amostra,
        P("Com **grupos muito desbalanceados**, a ROC e a AUC podem parecer boas enquanto o grupo raro é mal previsto: a taxa de falsos positivos se dilui no grupo grande.",
          verificar = "multi/confusion",
          se_falhar = "Leia a sensibilidade de cada grupo no `multi/confusion`; curva precisão-revocação ainda sem bloco no trama (lacuna registrada)."),
        P("Com três ou mais grupos, cada curva é **um grupo contra os outros**: a AUC de cada uma não soma nem resume o classificador inteiro."),
        P("A AUC sai **sem intervalo de confiança**: com poucos positivos ela varia muito de amostra para amostra.",
          se_falhar = "Intervalo da AUC (Hanley-McNeil, DeLong) ainda sem bloco no trama (lacuna registrada).")),
      referencias = list(hanley, fawcett, saito,
        I("trama.multi", "tr_multi_roc", "Implementação própria: um ponto por escore distinto e AUC de Mann-Whitney (meio ponto por empate); probabilidades por deixa-um-fora como no `multi/confusion`.")))
  )
}
