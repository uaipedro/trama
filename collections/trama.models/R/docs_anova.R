# Pressupostos e referências: delineamentos e quadro da ANOVA.

.tr_models_docs_anova <- function() {
  P <- .tr_models_P; I <- .tr_models_impl
  L <- .tr_models_livros()
  casualizacao <- function(como) P(
    sprintf("Os tratamentos foram **sorteados** %s, e cada parcela é uma unidade experimental independente.", como),
    se_falhar = "Não há teste para isso: é o planejamento que garante. Sem sorteio, o F não tem a interpretação causal do experimento.")
  normal_p <- function(alternativa) P("Os **erros** (resíduos) são normais — não a resposta crua.",
              verificar = c("models/shapiro_residuals", "models/plot_diagnostics"),
              se_falhar = paste("Transforme a resposta (log, raiz) ou, para contagens e proporções, use o `models/glm`;", alternativa))
  normal_dic <- normal_p("sem modelo que sirva, o `models/kruskal`.")
  normal_dbc <- normal_p("sem modelo que sirva, o `models/friedman` (uma observação por bloco e tratamento).")
  normal <- normal_p("não há teste por postos para este delineamento no trama (o `models/friedman` é só para o DBC de um fator).")
  homog_p <- function(verificar) P("A **variância do erro é a mesma** em todos os tratamentos (homocedasticidade).",
             verificar = verificar,
             se_falhar = "Transforme a resposta (o log quando a variância cresce com a média) ou use o `models/glm` com família gama.")
  homog_dic <- homog_p(c("models/levene", "models/bartlett", "models/plot_diagnostics"))
  # Com bloco, só o Levene: o de O'Neill & Mathews corrige a correlação dos
  # resíduos, e o Bartlett recusa.
  homog <- homog_p(c("models/levene", "models/plot_diagnostics"))
  aditiv <- P("**Aditividade**: o efeito do tratamento é o mesmo em todo bloco (não há interação bloco × tratamento).",
              verificar = "models/tukey_additivity",
              se_falhar = "Uma transformação (quase sempre o log) costuma devolver a aditividade.")
  aov_nota <- "`aov` com os fatores do desenho convertidos em fator; o quadro é a soma de quadrados sequencial (tipo I), que no balanceado coincide com as outras."
  list(
    "models/anova_dic" = list(
      pressupostos = list(casualizacao("nas parcelas sem restrição"),
        P("As parcelas são **homogêneas**: não há fonte de variação conhecida (área, dia, lote) que devesse ter virado bloco.",
          se_falhar = "Se houve bloco, use o `models/anova_dbc`."),
        normal_dic, homog_dic),
      referencias = list(L$banzatto, L$pimentel, L$montgomery, I("stats", "aov", aov_nota))),

    "models/anova_dbc" = list(
      pressupostos = list(casualizacao("dentro de cada bloco, com todos os tratamentos em todo bloco"),
        aditiv, normal_dbc, homog),
      referencias = list(L$banzatto, L$pimentel, L$montgomery, I("stats", "aov", aov_nota))),

    "models/anova_dql" = list(
      pressupostos = list(casualizacao("com cada tratamento uma vez em cada linha e em cada coluna"),
        P("**Aditividade** de linha, coluna e tratamento: nenhuma interação entre eles.",
          verificar = "models/tukey_additivity",
          se_falhar = "Transforme a resposta; o delineamento não tem gl para estimar essas interações."),
        normal, homog),
      referencias = list(L$banzatto, L$pimentel, L$montgomery, I("stats", "aov", aov_nota))),

    "models/anova_factorial" = list(
      pressupostos = list(casualizacao("entre as combinações dos fatores (nas parcelas, ou dentro dos blocos)"),
        P("Com bloco: **aditividade** entre bloco e as combinações de tratamento.",
          verificar = "models/tukey_additivity"),
        normal, homog,
        P("Todas as combinações dos fatores foram observadas; com parcelas perdidas, o quadro sequencial depende da ordem dos fatores.",
          verificar = "models/anova_table",
          se_falhar = "No desbalanceado, leia o `models/anova_table` com SQ tipo II ou III e compare médias ajustadas no `models/emmeans`.")),
      referencias = list(L$banzatto, L$pimentel, L$montgomery, I("stats", "aov", aov_nota))),

    "models/anova_split_plot" = list(
      pressupostos = list(
        P("Houve **dois sorteios**: o fator da parcela nas parcelas de cada bloco, e o da subparcela dentro de cada parcela. É isso que justifica os dois erros.",
          se_falhar = "Se os dois fatores foram sorteados juntos nas parcelas, é fatorial: use o `models/anova_factorial`."),
        P("Os erros (a) e (b) são **normais e independentes** entre si, cada um com variância constante.",
          verificar = c("models/shapiro_residuals", "models/plot_diagnostics"),
          se_falhar = "Transforme a resposta. O Shapiro usa os resíduos do erro (b); Levene e Bartlett recusam a parcela subdividida (sem correção publicada para dois estratos de erro), e a variância se lê no painel escala-locação."),
        P("Dados **balanceados** (todo bloco com todas as combinações): o quadro com `Error()` só é o dos livros assim.",
          se_falhar = "No desbalanceado, ajuste o misto `(1 | bloco:parcela)` no `models/lmer`."),
        P("Se a subparcela é **tempo** ou medida repetida (não sorteada dentro da parcela), a análise com dois erros supõe **esfericidade** (simetria composta): a correlação entre duas medidas da mesma parcela é igual para qualquer par de tempos.",
          se_falhar = "Sem esfericidade (tempos próximos mais correlacionados que distantes), o F da subparcela fica liberal. O `models/lmer` só aceita efeitos aleatórios, não uma estrutura de correlação no erro (como AR(1)); é lacuna registrada na revisão metodológica.")),
      referencias = list(L$banzatto, L$pimentel, L$montgomery,
        I("stats", "aov", "Com `Error(bloco/parcela)`: o quadro sai dos estratos, com o bloco testado contra o erro (a). Médias e comparações usam o misto equivalente em `lmerTest::lmer` com `(1 | bloco:parcela)`."))),

    "models/anova_table" = list(
      pressupostos = list(
        P("Os pressupostos do **modelo** que entra (normalidade e variância constante do erro nos modelos lineares; família e ligação no GLM) valem: o quadro só lê o ajuste.",
          verificar = c("models/shapiro_residuals", "models/levene", "models/plot_diagnostics")),
        P("No **tipo III**, os fatores estão em contrastes de soma zero (o bloco reajusta sozinho, também no GLM misto e no GLS; o `lmerTest` já é invariante ao contraste). Uma **covariável numérica em interação** com fator não é centrada: o efeito principal do fator é testado com a covariável em zero, e a nota avisa. No **tipo I**, a ordem dos termos na fórmula é a ordem das perguntas.",
          se_falhar = "No desbalanceado, prefira o tipo II quando não há interação, e o III quando há.")),
      referencias = list(L$searle, L$fox, L$montgomery,
        I("nlme", "anova.gls", "No GLS, `type = \"sequential\"` (tipo I) ou `\"marginal\"` (tipo III, com os fatores reajustados em `contr.sum`)."),
        I("stats", "anova", "Tipo I (`lm`, `aov`, `glm`: F ou qui-quadrado conforme a dispersão da família); no misto, `anova` do `lmerTest` com gl de Satterthwaite."),
        I("car", "Anova", "Tipos II e III (`type = 2` ou `3`); no tipo III o modelo (também o `glmer`) é reajustado com `contr.sum`. No GLM, `test.statistic = \"F\"` ou `\"LR\"`.")))
  )
}
