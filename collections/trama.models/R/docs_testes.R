# Pressupostos e referências: testes de pressupostos e testes clássicos.

.tr_models_docs_testes <- function() {
  P <- .tr_models_P; I <- .tr_models_impl; R <- trama::tr_ref
  L <- .tr_models_livros()
  indep <- function(unidade = "observações") P(
    sprintf("As %s são **independentes** entre si.", unidade),
    se_falhar = "Não há teste no trama para isso: é o delineamento (sorteio, uma medida por unidade) que garante. Medidas repetidas na mesma unidade pedem `models/paired_t` ou `models/lmer`.")
  shapiro_wilk <- R(autores = c("Shapiro, S. S.", "Wilk, M. B."), ano = 1965,
                    titulo = "An analysis of variance test for normality (complete samples)",
                    fonte = "Biometrika, 52(3-4), 591-611", doi = "10.1093/biomet/52.3-4.591")
  student <- R(autores = "Student", ano = 1908, titulo = "The probable error of a mean",
               fonte = "Biometrika, 6(1), 1-25", doi = "10.2307/2331554")
  wilcoxon <- R(autores = "Wilcoxon, F.", ano = 1945, titulo = "Individual comparisons by ranking methods",
                fonte = "Biometrics Bulletin, 1(6), 80-83", doi = "10.2307/3001968")
  residuos_do_modelo <- "Os resíduos vêm de um modelo **bem especificado** (termos certos, sem tendência sobrando): o teste olha os resíduos, e um modelo errado muda o que eles medem."
  list(
    "models/shapiro_residuals" = list(
      pressupostos = list(
        P(residuos_do_modelo, verificar = "models/plot_diagnostics",
          se_falhar = "Acerte o modelo antes (termo faltante, interação, curvatura) e teste de novo."),
        P("De **3 a 5000** resíduos, e não muito arredondados (muitos empates distorcem o W).",
          se_falhar = "Acima de 5000, leia o Q-Q do `models/plot_diagnostics`: com esse n qualquer desvio mínimo rejeita.")),
      referencias = list(shapiro_wilk,
        I("stats", "shapiro.test", "Aplicado aos resíduos do ajuste: erro (b) na parcela subdividida, resíduos condicionais no misto."))),

    "models/levene" = list(
      pressupostos = list(
        P("Os grupos (as combinações dos tratamentos) têm **pelo menos duas observações** cada; o bloco recusa senão."),
        indep("observações de cada grupo"),
        P(residuos_do_modelo, verificar = "models/plot_diagnostics"),
        P("Com bloco (DBC, fatorial em DBC, DQL), o delineamento é **equilibrado** (sem parcela perdida): a correção de O'Neill & Mathews é um fator do desenho que supõe o equilíbrio; o bloco recusa senão. Ali o centro é o ajuste de mínimos quadrados (a média), qualquer que seja o param Centro.",
          se_falhar = "Leia o painel escala-locação do `models/plot_diagnostics`."),
        P("O modelo **não é parcela subdividida**: os resíduos de dois estratos de erro não têm correção publicada (a de O'Neill & Mathews supõe um estrato), e o bloco recusa.",
          se_falhar = "Leia o painel escala-locação do `models/plot_diagnostics`; com variâncias diferentes, ajuste o misto no `models/lmer`.")),
      referencias = list(
        R(autores = c("O'Neill, M. E.", "Mathews, K. L."), ano = 2002,
          titulo = "Levene tests of homogeneity of variance for general block and treatment designs",
          fonte = "Biometrics, 58(1), 216-224",
          doi = "10.1111/j.0006-341x.2002.00216.x"),
        R(autores = c("O'Neill, M. E.", "Mathews, K."), ano = 2000,
          titulo = "A weighted least squares approach to Levene's test of homogeneity of variance",
          fonte = "Australian & New Zealand Journal of Statistics, 42(1), 81-100",
          doi = "10.1111/1467-842x.00109"),
        R(autores = "Levene, H.", ano = 1960, titulo = "Robust tests for equality of variances",
          fonte = "In: Olkin, I. et al. (ed.). Contributions to Probability and Statistics: Essays in Honor of Harold Hotelling. Stanford: Stanford University Press, p. 278-292"),
        R(autores = c("Brown, M. B.", "Forsythe, A. B."), ano = 1974,
          titulo = "Robust tests for the equality of variances",
          fonte = "Journal of the American Statistical Association, 69(346), 364-367",
          doi = "10.1080/01621459.1974.10482955"),
        L$montgomery,
        I("car", "leveneTest", "Sem bloco: nos resíduos do modelo, agrupados pelos tratamentos. `center = median` (padrão do bloco) é a versão de Brown-Forsythe; `center = mean` é o Levene original. Com bloco (DBC, DQL) a conta é própria (O'Neill & Mathews 2002), validada contra o `oneilldbc` do ExpDes.pt."))),

    "models/bartlett" = list(
      pressupostos = list(
        P("Os resíduos de cada grupo são **normais**: o teste é muito sensível à falta de normalidade, e rejeita pela cauda, não pela variância.",
          verificar = c("models/shapiro_residuals", "models/plot_diagnostics"),
          se_falhar = "Use o `models/levene` (centro na mediana), robusto à falta de normalidade."),
        P("Os grupos têm **pelo menos duas observações** cada."),
        P("O modelo **não tem bloco** (DIC, fatorial em DIC, modelos de fórmula): nos resíduos correlacionados de um DBC, DQL ou parcela subdividida o Bartlett não tem correção publicada, e o bloco recusa.",
          se_falhar = "Use o `models/levene`, que no delineamento com bloco aplica a correção de O'Neill & Mathews (2002)."),
        indep("observações de cada grupo")),
      referencias = list(
        R(autores = "Bartlett, M. S.", ano = 1937, titulo = "Properties of sufficiency and statistical tests",
          fonte = "Proceedings of the Royal Society of London. Series A, 160(901), 268-282",
          doi = "10.1098/rspa.1937.0109"),
        L$montgomery,
        I("stats", "bartlett.test", "Nos resíduos do modelo, agrupados pelos tratamentos."))),

    "models/breusch_pagan" = list(
      pressupostos = list(
        P("A heterocedasticidade, se existe, é **função dos preditores do modelo** (o teste regride o quadrado do resíduo neles); uma variância que muda com outra coisa (o tempo, uma variável fora do modelo) passa despercebida.",
          verificar = "models/plot_diagnostics"),
        P(residuos_do_modelo, verificar = "models/plot_diagnostics"),
        P("Amostra **grande o bastante** para a aproximação qui-quadrado de n·R².")),
      referencias = list(
        R(autores = c("Breusch, T. S.", "Pagan, A. R."), ano = 1979,
          titulo = "A simple test for heteroscedasticity and random coefficient variation",
          fonte = "Econometrica, 47(5), 1287-1294", doi = "10.2307/1911963"),
        R(autores = "Koenker, R.", ano = 1981, titulo = "A note on studentizing a test for heteroscedasticity",
          fonte = "Journal of Econometrics, 17(1), 107-112", doi = "10.1016/0304-4076(81)90062-2"),
        I("trama.models", "tr_models_breusch_pagan", "Cálculo próprio: regressão auxiliar com `stats::lm.fit` do quadrado dos resíduos na matriz do modelo, estatística n·R² (versão studentizada de Koenker, a mesma do padrão de `lmtest::bptest`)."))),

    "models/tukey_additivity" = list(
      pressupostos = list(
        P("A não aditividade, se existe, tem a forma **multiplicativa** que o teste procura (efeito proporcional ao produto dos efeitos de bloco e tratamento); outras formas de interação podem passar."),
        P("Os erros do modelo aditivo são **normais e homocedásticos**: o teste é um F.",
          verificar = c("models/shapiro_residuals", "models/levene"))),
      referencias = list(
        R(autores = "Tukey, J. W.", ano = 1949, titulo = "One degree of freedom for non-additivity",
          fonte = "Biometrics, 5(3), 232-242", doi = "10.2307/3001938"),
        L$montgomery,
        I("trama.models", "tr_models_tukey_additivity", "Cálculo próprio com `stats::lm` e `stats::anova`: acrescenta o quadrado do valor ajustado ao modelo aditivo (bloco + tratamento, ou linha + coluna + tratamento no DQL) e testa esse termo com 1 gl."))),

    "models/t_test" = list(
      pressupostos = list(
        indep("observações, dentro e entre os dois grupos,"),
        P("A resposta é **aproximadamente normal** em cada grupo (com amostras grandes, o teorema central do limite alivia). O `models/shapiro` lê uma coluna só: para conferir por grupo, filtre cada grupo (`data/filter`) e rode `models/shapiro`, ou olhe o `view/qq`.",
          verificar = c("models/shapiro", "view/qq"),
          se_falhar = "Use o `models/wilcoxon`."),
        P("No t de **Student** (variâncias iguais ligado), as variâncias dos dois grupos são iguais. O de Welch, padrão, não pede isso.",
          se_falhar = "Deixe **Variâncias iguais** desligado (Welch).")),
      referencias = list(student,
        R(autores = "Welch, B. L.", ano = 1947,
          titulo = "The generalization of 'Student's' problem when several different population variances are involved",
          fonte = "Biometrika, 34(1-2), 28-35", doi = "10.1093/biomet/34.1-2.28"),
        L$montgomery,
        I("stats", "t.test", "`var.equal = FALSE` por padrão (Welch, gl de Welch-Satterthwaite); `TRUE` dá o t de Student com variância combinada."))),

    "models/paired_t" = list(
      pressupostos = list(
        P("Os pares são **independentes** entre si (as duas medidas de um par é que são ligadas).",
          se_falhar = "É o delineamento que garante; unidades agrupadas pedem `models/lmer`."),
        P("As **diferenças** (primeira − segunda) são aproximadamente normais — não cada medida.",
          verificar = c("models/shapiro", "view/qq"),
          se_falhar = "Calcule a diferença num `data/mutate` e confira; sem normalidade, o Wilcoxon de postos sinalizados é a alternativa (ainda não há bloco no trama).")),
      referencias = list(student, L$montgomery,
        I("stats", "t.test", "Aplicado às diferenças, como t para uma amostra com média zero."))),

    "models/one_sample_t" = list(
      pressupostos = list(
        indep(),
        P("A coluna é **aproximadamente normal** (com amostras grandes, o teorema central do limite alivia). Se a coluna mistura grupos, filtre cada grupo (`data/filter`) e rode o `models/shapiro`, ou olhe o `view/qq`.",
          verificar = c("models/shapiro", "view/qq"))),
      referencias = list(student, L$montgomery,
        I("stats", "t.test", "Com `mu` = valor de referência."))),

    "models/wilcoxon" = list(
      pressupostos = list(
        indep("observações, dentro e entre os dois grupos,"),
        P("A resposta é **contínua** (ou ao menos ordinal com poucos empates); com empates o p-valor é o da aproximação normal, e a nota avisa."),
        P("Para ler o efeito como **deslocamento** de locação (Hodges-Lehmann), as duas distribuições têm a **mesma forma**; sem isso o teste compara P(X > Y), não medianas.",
          verificar = c("view/boxplot", "view/density"))),
      referencias = list(wilcoxon,
        R(autores = c("Mann, H. B.", "Whitney, D. R."), ano = 1947,
          titulo = "On a test of whether one of two random variables is stochastically larger than the other",
          fonte = "The Annals of Mathematical Statistics, 18(1), 50-60", doi = "10.1214/aoms/1177730491"),
        R(autores = c("Hodges, J. L.", "Lehmann, E. L."), ano = 1963, titulo = "Estimates of location based on rank tests",
          fonte = "The Annals of Mathematical Statistics, 34(2), 598-611", doi = "10.1214/aoms/1177704172",
          papel = "complementar"),
        L$siegel,
        I("stats", "wilcox.test", "`conf.int = TRUE` (estimador de Hodges-Lehmann); p-valor exato sem empates e n < 50, aproximação normal com correção de continuidade nos demais."))),

    "models/kruskal" = list(
      pressupostos = list(
        indep("observações, dentro e entre os grupos,"),
        P("A resposta é **contínua** ou ordinal; empates são corrigidos na estatística."),
        P("Para ler a rejeição como diferença de **locação** (medianas), as distribuições dos grupos têm a **mesma forma**.",
          verificar = c("view/boxplot", "view/density"))),
      referencias = list(
        R(autores = c("Kruskal, W. H.", "Wallis, W. A."), ano = 1952,
          titulo = "Use of ranks in one-criterion variance analysis",
          fonte = "Journal of the American Statistical Association, 47(260), 583-621",
          doi = "10.1080/01621459.1952.10483441"),
        L$siegel,
        I("stats", "kruskal.test", "H com correção para empates, p-valor pela aproximação qui-quadrado."))),

    "models/friedman" = list(
      pressupostos = list(
        P("Os **blocos são independentes** entre si; dentro do bloco, cada tratamento foi sorteado a uma parcela."),
        P("**Uma observação por bloco e tratamento**, com todos os tratamentos em todo bloco.",
          se_falhar = "Resuma as repetições (a média de cada casela) antes; bloco incompleto sai inteiro."),
        P("A resposta é **contínua** ou ordinal; empates dentro do bloco são corrigidos na estatística."),
        P("O p-valor é o da aproximação qui-quadrado, boa com blocos e tratamentos não muito poucos (com 3 tratamentos, uns 10 blocos).")),
      referencias = list(
        R(autores = "Friedman, M.", ano = 1937,
          titulo = "The use of ranks to avoid the assumption of normality implicit in the analysis of variance",
          fonte = "Journal of the American Statistical Association, 32(200), 675-701",
          doi = "10.1080/01621459.1937.10503522"),
        L$siegel,
        I("stats", "friedman.test", "Postos dentro do bloco, estatística corrigida para empates, p-valor pela aproximação qui-quadrado; o W de Kendall é a estatística dividida por b(k - 1)."))),

    "models/chisq" = list(
      pressupostos = list(
        P("Cada linha da tabela é uma **unidade independente**, contada uma vez só.",
          se_falhar = "É a coleta que garante; a mesma unidade em duas caselas invalida o teste."),
        P("Contagens **esperadas** de pelo menos 5 em (quase) todas as caselas: com mais de 20% abaixo disso, a aproximação qui-quadrado falha.",
          se_falhar = "A própria nota do bloco avisa quando há esperados abaixo de 5 e sugere o `models/fisher_exact`; outra saída é juntar categorias raras num `data/mutate`.")),
      referencias = list(
        R(autores = "Pearson, K.", ano = 1900,
          titulo = "On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling",
          fonte = "The London, Edinburgh, and Dublin Philosophical Magazine and Journal of Science, 50(302), 157-175",
          doi = "10.1080/14786440009463897"),
        R(autores = "Yates, F.", ano = 1934, titulo = "Contingency tables involving small numbers and the χ² test",
          fonte = "Supplement to the Journal of the Royal Statistical Society, 1(2), 217-235",
          doi = "10.2307/2983604", papel = "complementar"),
        R(autores = "Agresti, A.", ano = 2002, titulo = "Categorical Data Analysis", fonte = "2. ed. Hoboken: Wiley",
          doi = "10.1002/0471249688", papel = "complementar"),
        L$siegel,
        I("stats", "chisq.test", "`correct = FALSE` por padrão no bloco: X² de Pearson sem correção, porque a de Yates deixa o teste conservador (Agresti 2002); a opção **Correção de Yates** liga `correct = TRUE`, que só age em 2 × 2. Validado contra a forma fechada do 2 × 2 no exemplo do Physicians' Health Study (Agresti)."))),

    "models/fisher_exact" = list(
      pressupostos = list(
        P("Cada linha da tabela é uma **unidade independente**, contada uma vez só."),
        P("Os **totais marginais** são tratados como fixos: o teste é condicional a eles (vale também quando não foram fixados no delineamento, mas fica conservador).")),
      referencias = list(
        R(autores = "Fisher, R. A.", ano = 1922,
          titulo = "On the interpretation of χ² from contingency tables, and the calculation of P",
          fonte = "Journal of the Royal Statistical Society, 85(1), 87-94", doi = "10.2307/2340521"),
        L$siegel,
        I("stats", "fisher.test", "`workspace = 2e6`; em 2 × 2, a razão de chances é a estimativa condicional de máxima verossimilhança, com IC exato."))),

    "models/cor_test" = list(
      pressupostos = list(
        indep("pares (x, y)"),
        P("**Pearson**: a relação é **linear**, e o teste supõe normalidade bivariada. O `models/shapiro` só confere a normalidade de cada variável sozinha (marginal): passar nas duas não garante a bivariada; olhe também a nuvem no `view/points`.",
          verificar = c("view/points", "models/shapiro"),
          se_falhar = "Use `spearman` ou `kendall` (relação monotônica, por postos)."),
        P("**Spearman** e **Kendall**: a relação é **monotônica** (sempre cresce ou sempre decresce).",
          verificar = "view/points")),
      referencias = list(
        R(autores = "Pearson, K.", ano = 1896,
          titulo = "Mathematical contributions to the theory of evolution. III. Regression, heredity, and panmixia",
          fonte = "Philosophical Transactions of the Royal Society of London. Series A, 187, 253-318",
          doi = "10.1098/rsta.1896.0007"),
        R(autores = "Spearman, C.", ano = 1904, titulo = "The proof and measurement of association between two things",
          fonte = "The American Journal of Psychology, 15(1), 72-101", doi = "10.2307/1412159"),
        R(autores = "Kendall, M. G.", ano = 1938, titulo = "A new measure of rank correlation",
          fonte = "Biometrika, 30(1-2), 81-93", doi = "10.1093/biomet/30.1-2.81"),
        L$siegel,
        I("stats", "cor.test", "IC de Pearson pela transformação z de Fisher; Spearman e Kendall sem IC, p-valor exato ou aproximado conforme n e empates."))),

    "models/shapiro" = list(
      pressupostos = list(
        indep(),
        P("De **3 a 5000** valores, sem muitos empates por arredondamento."),
        P("A coluna é **uma população só**: com grupos ou tratamentos misturados, a coluna crua não é o que um modelo supõe normal.",
          se_falhar = "Para os pressupostos de ANOVA ou regressão, use o `models/shapiro_residuals`.")),
      referencias = list(shapiro_wilk,
        I("stats", "shapiro.test")))
  )
}
