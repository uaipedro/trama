# Pressupostos e referências: médias ajustadas e comparações múltiplas.

.tr_models_docs_medias <- function() {
  P <- .tr_models_P; I <- .tr_models_impl; R <- trama::tr_ref
  L <- .tr_models_livros()
  modelo_ok <- P("O **modelo** que entra atende aos seus pressupostos (erros normais, variância constante, independência): as comparações usam o erro padrão dele.",
                 verificar = c("models/shapiro_residuals", "models/levene", "models/plot_diagnostics"),
                 se_falhar = "Acerte o modelo antes: transformação, `models/glm`, ou `models/lmer` para erros correlacionados.")
  interacao <- P("Sem **interação** relevante com outro fator; havendo, a média de um fator somada sobre o outro esconde o que aconteceu.",
                 verificar = "models/anova_table",
                 se_falhar = "Compare dentro de cada nível do outro fator (**Por** no `models/emmeans`).")
  anova_agricolae <- P("As médias vêm de uma **ANOVA** com um erro só por fator (o bloco escolhe o erro (a) ou (b) na parcela subdividida), e os dados são **balanceados**: as médias usadas são as da tabela.",
                       se_falhar = "No desbalanceado, ou em GLM e misto, use o `models/emmeans`.")
  searle80 <- R(autores = c("Searle, S. R.", "Speed, F. M.", "Milliken, G. A."), ano = 1980,
                titulo = "Population marginal means in the linear model: an alternative to least squares means",
                fonte = "The American Statistician, 34(4), 216-221", doi = "10.1080/00031305.1980.10483031")
  tukey49 <- R(autores = "Tukey, J. W.", ano = 1949, titulo = "Comparing individual means in the analysis of variance",
               fonte = "Biometrics, 5(2), 99-114", doi = "10.2307/3001913", papel = "complementar")
  kramer <- R(autores = "Kramer, C. Y.", ano = 1956,
              titulo = "Extension of multiple range tests to group means with unequal numbers of replications",
              fonte = "Biometrics, 12(3), 307-310", doi = "10.2307/3001469")
  dunnett <- R(autores = "Dunnett, C. W.", ano = 1955,
               titulo = "A multiple comparison procedure for comparing several treatments with a control",
               fonte = "Journal of the American Statistical Association, 50(272), 1096-1121",
               doi = "10.1080/01621459.1955.10501294")
  list(
    "models/emmeans" = list(
      pressupostos = list(modelo_ok, interacao,
        P("As médias ajustadas dão **peso igual** a cada nível dos outros fatores; é a população de referência que a comparação supõe.")),
      referencias = list(searle80, kramer,
        R(autores = "Piepho, H.-P.", ano = 2004,
          titulo = "An algorithm for a letter-based representation of all-pairwise comparisons",
          fonte = "Journal of Computational and Graphical Statistics, 13(2), 456-466",
          doi = "10.1198/1061860043515", papel = "complementar"),
        L$montgomery,
        I("emmeans", "emmeans", "Grade de referência com pesos iguais; no misto e na parcela subdividida, `lmer.df = \"satterthwaite\"`; no GLM, `type = \"response\"` na escala da resposta."),
        I("emmeans", "contrast", "`\"pairwise\"` com o `adjust` escolhido (Tukey = Tukey-Kramer no desbalanceado), dentro de cada nível de **Por**; as letras saem do algoritmo de inserir e absorver, implementado no trama."))),

    "models/pairwise" = list(
      pressupostos = list(modelo_ok, interacao,
        P("As comparações foram **escolhidas antes** de ver os dados (todos os pares, ou todos contra o controle): o ajuste protege essa família, e não comparações escolhidas depois.")),
      referencias = list(tukey49, kramer, dunnett, L$montgomery,
        I("emmeans", "contrast", "`\"pairwise\"` ou `\"trt.vs.ctrl\"`; o ajuste `dunnett` usa `adjust = \"dunnettx\"`, a aproximação de Hsu para o Dunnett."))),

    "models/linear_hypothesis" = list(
      pressupostos = list(modelo_ok,
        P("Os contrastes foram **planejados** antes de ver os dados, e são linearmente independentes (o bloco recusa os dependentes).",
          se_falhar = "Para comparações sugeridas pelos dados, use o `models/pairwise` com ajuste."),
        P("No GLM e no misto, o teste é **assintótico** (Wald) ou aproximado (Satterthwaite): pede amostra grande o bastante.")),
      referencias = list(L$searle, L$fox,
        I("emmeans", "contrast", "Contrastes nas médias de um fator; F conjunto calculado pela forma quadrática de Wald com os gl do `emmeans::test(joint = TRUE)`."),
        I("car", "linearHypothesis", "Hipóteses nos coeficientes (`lm`, `glm`): F, ou qui-quadrado de Wald nas famílias de dispersão fixa."),
        I("lmerTest", "contest", "Hipóteses nos coeficientes do misto, `joint = TRUE`, gl de Satterthwaite."))),

    "models/duncan" = list(
      pressupostos = list(modelo_ok, anova_agricolae, interacao,
        P("Aceita-se uma taxa de erro por família **maior** que a nominal: o nível de proteção cai com o número de médias entre as comparadas.",
          se_falhar = "Para controlar o erro da família, use o Tukey do `models/emmeans`.")),
      referencias = list(
        R(autores = "Duncan, D. B.", ano = 1955, titulo = "Multiple range and multiple F tests",
          fonte = "Biometrics, 11(1), 1-42", doi = "10.2307/3001478"),
        L$banzatto,
        I("agricolae", "duncan.test", "Com o QM e os gl do erro do quadro (`DFerror`, `MSerror`) e `group = TRUE`."))),

    "models/waller_duncan" = list(
      pressupostos = list(modelo_ok, anova_agricolae,
        P("Os efeitos de tratamento são tratados como **intercambiáveis** a priori (a regra é bayesiana), e a razão K reflete o custo relativo dos erros tipo I e II.")),
      referencias = list(
        R(autores = c("Waller, R. A.", "Duncan, D. B."), ano = 1969,
          titulo = "A Bayes rule for the symmetric multiple comparisons problem",
          fonte = "Journal of the American Statistical Association, 64(328), 1484-1503",
          doi = "10.1080/01621459.1969.10501073"),
        I("agricolae", "waller.test", "Com o QM e os gl do erro, o F do tratamento (`Fc`) e a razão `K`; `group = TRUE`.")))
  )
}
