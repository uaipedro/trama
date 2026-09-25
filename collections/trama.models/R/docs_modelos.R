# Pressupostos e referências: modelos de fórmula livre e comparação de modelos.

.tr_models_docs_modelos <- function() {
  P <- .tr_models_P; I <- .tr_models_impl; R <- trama::tr_ref
  L <- .tr_models_livros()
  wilks <- R(autores = "Wilks, S. S.", ano = 1938,
             titulo = "The large-sample distribution of the likelihood ratio for testing composite hypotheses",
             fonte = "The Annals of Mathematical Statistics, 9(1), 60-62", doi = "10.1214/aoms/1177732360")
  self_liang <- R(autores = c("Self, S. G.", "Liang, K.-Y."), ano = 1987,
                  titulo = "Asymptotic properties of maximum likelihood estimators and likelihood ratio tests under nonstandard conditions",
                  fonte = "Journal of the American Statistical Association, 82(398), 605-610",
                  doi = "10.1080/01621459.1987.10478472", papel = "complementar")
  lme4 <- R(autores = c("Bates, D.", "Mächler, M.", "Bolker, B.", "Walker, S."), ano = 2015,
            titulo = "Fitting linear mixed-effects models using lme4",
            fonte = "Journal of Statistical Software, 67(1), 1-48", doi = "10.18637/jss.v067.i01")
  lmertest <- R(autores = c("Kuznetsova, A.", "Brockhoff, P. B.", "Christensen, R. H. B."), ano = 2017,
                titulo = "lmerTest package: tests in linear mixed effects models",
                fonte = "Journal of Statistical Software, 82(13), 1-26", doi = "10.18637/jss.v082.i13")
  linear <- P("A relação entre a resposta e os preditores é a da **fórmula** (linear nos parâmetros, com as interações e curvaturas escritas).",
              verificar = "models/plot_diagnostics",
              se_falhar = "Acrescente termos (`poly(x, 2)`, `a * b`) ou transforme; teste o termo a mais com o `models/compare`.")
  normal <- P("Os **erros** são normais.", verificar = c("models/shapiro_residuals", "models/plot_diagnostics"),
              se_falhar = "Transforme a resposta, ou use o `models/glm` com a família da resposta.")
  homog <- P("A **variância do erro é constante** (não cresce com o ajustado nem muda entre grupos).",
             verificar = c("models/breusch_pagan", "models/levene", "models/plot_diagnostics"),
             se_falhar = "Transforme a resposta (log) ou use o `models/glm` com família gama.")
  indep <- P("As observações são **independentes**.",
             se_falhar = "Medidas repetidas ou agrupadas (mesmo animal, parcela, local) pedem o `models/lmer`.")
  list(
    "models/lm" = list(
      pressupostos = list(linear, indep, normal, homog,
        P("Sem **colinearidade** forte entre os preditores: com ela os coeficientes ficam instáveis e os erros padrão, grandes.",
          verificar = "models/coefficients")),
      referencias = list(L$rencher, L$montgomery,
        I("stats", "lm", "Mínimos quadrados; texto e lógico viram fator (contraste de tratamento, o padrão do R), número fica número."))),

    "models/glm" = list(
      pressupostos = list(indep,
        P("A resposta segue a **família** escolhida, e a média se liga aos preditores pela **ligação** (canônica; log na gama).",
          verificar = c("models/plot_diagnostics", "models/residuals")),
        P("Na binomial e na Poisson, a variância é a da família: **sem superdispersão** (desvio residual perto dos gl do resíduo).",
          verificar = "models/fit_stats",
          se_falhar = "Na Poisson, use a família `quasipoisson`; na binomial agregada (`cbind(sucessos, fracassos)`), a `quasibinomial`. As duas estimam a dispersão pelo X² de Pearson / gl, e os testes passam a F. Em dados 0/1 (binomial não agregada), desvio perto dos gl não diz nada sobre superdispersão."),
        P("Amostra **grande o bastante**: os testes e intervalos de um GLM são assintóticos.")),
      referencias = list(
        R(autores = c("Nelder, J. A.", "Wedderburn, R. W. M."), ano = 1972, titulo = "Generalized linear models",
          fonte = "Journal of the Royal Statistical Society. Series A, 135(3), 370-384", doi = "10.2307/2344614"),
        R(autores = "Wedderburn, R. W. M.", ano = 1974,
          titulo = "Quasi-likelihood functions, generalized linear models, and the Gauss-Newton method",
          fonte = "Biometrika, 61(3), 439-447", doi = "10.1093/biomet/61.3.439", papel = "complementar"),
        L$dobson,
        I("stats", "glm", "Famílias `gaussian`, `binomial`, `poisson`, `quasipoisson`, `quasibinomial` na ligação canônica e `Gamma(link = \"log\")`; nas quasi, dispersão pelo X² de Pearson / gl e quadro com F."))),

    "models/lmer" = list(
      pressupostos = list(linear,
        P("Os **efeitos aleatórios** são normais, com média zero, e independentes do erro.",
          verificar = c("models/random_effects", "models/plot_caterpillar")),
        P("Os **erros** condicionais (resíduos depois de descontar os efeitos aleatórios) são normais e de variância constante. Levene e Breusch-Pagan não se aplicam a esses resíduos; a conferência é visual.",
          verificar = "models/plot_diagnostics"),
        P("Os **grupos** (sujeitos, blocos) são independentes entre si, e há grupos suficientes para estimar cada variância.",
          se_falhar = "Com poucos grupos (menos de 5 ou 6), trate o fator como fixo no `models/lm`.")),
      referencias = list(lme4, lmertest,
        R(autores = c("Patterson, H. D.", "Thompson, R."), ano = 1971,
          titulo = "Recovery of inter-block information when block sizes are unequal",
          fonte = "Biometrika, 58(3), 545-554", doi = "10.1093/biomet/58.3.545", papel = "complementar"),
        R(autores = "Satterthwaite, F. E.", ano = 1946,
          titulo = "An approximate distribution of estimates of variance components",
          fonte = "Biometrics Bulletin, 2(6), 110-114", doi = "10.2307/3002019", papel = "complementar"),
        I("lmerTest", "lmer", "Mesmo ajuste do `lme4::lmer`, com gl de Satterthwaite nos testes; `REML = TRUE` por padrão."))),

    "models/compare" = list(
      pressupostos = list(
        P("Os modelos são **aninhados** (o menor é um caso particular do maior), da mesma família e ajustados nas **mesmas linhas**; o bloco recusa senão.",
          se_falhar = "Para modelos não aninhados, compare o AIC no `models/fit_stats`."),
        P("O modelo **maior** atende aos pressupostos dele: o teste compara ajustes, não conserta um modelo errado.",
          verificar = c("models/plot_diagnostics", "models/shapiro_residuals")),
        P("No GLM e no misto, o teste é de **razão de verossimilhança** assintótico: pede amostra grande.")),
      referencias = list(wilks, L$rencher, L$dobson,
        I("stats", "anova", "`lm`: F de modelos aninhados; GLM: `test = \"F\"` nas famílias de dispersão estimada e `\"Chisq\"` nas demais; misto: `anova` do `lme4` com `refit = TRUE` (reajuste por ML)."))),

    "models/random_test" = list(
      pressupostos = list(
        P("O modelo misto atende aos seus pressupostos (efeitos aleatórios e erros normais).",
          verificar = c("models/plot_diagnostics", "models/plot_caterpillar")),
        P("A variância testada está na **fronteira** (zero) sob H0: a distribuição qui-quadrado não vale exatamente, e o p-valor é conservador.",
          se_falhar = "Um termo que sai significativo é seguro; um p-valor perto do limite pode estar superestimado.")),
      referencias = list(wilks, self_liang, lmertest,
        I("lmerTest", "ranova", "Retira cada termo aleatório (uma inclinação aleatória é reduzida ao intercepto) e compara por razão de verossimilhança."))),

    "models/rls" = list(
      pressupostos = list(
        P("O modelo é **linear** e seus coeficientes são **fixos** no tempo: o RLS sem fator de esquecimento pondera igualmente o passado e o presente.",
          se_falhar = "Para relação que muda com o tempo, ajuste por janelas, ou use o `models/lm` em cada período."),
        P("Os preditores estão em **escalas parecidas**: a prior difusa `lambda · I` (a matriz inicial `P0`, que diz ao RLS \"ainda não sei nada\" sobre os coeficientes) fica mal condicionada com colunas de ordens de grandeza diferentes.",
          se_falhar = "Escale as colunas num `data/mutate` antes."),
        P("Os mesmos pressupostos de erro do `models/lm` (independência, variância constante) valem para interpretar os coeficientes como os de mínimos quadrados.")),
      referencias = list(
        R(autores = "Plackett, R. L.", ano = 1950, titulo = "Some theorems in least squares",
          fonte = "Biometrika, 37(1-2), 149-157", doi = "10.1093/biomet/37.1-2.149"),
        I("trama.models", "step_rls", "Implementação própria da atualização recursiva de mínimos quadrados, com `P0 = lambda · I` e simetrização de `P` a cada passo.")))
  )
}
