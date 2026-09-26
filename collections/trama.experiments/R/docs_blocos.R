# Pressupostos e referências dos blocos, estruturados (`tr_pressuposto`,
# `tr_ref`), no molde do `.tr_models_doc` do trama.models: a página de ajuda e
# o site montam as seções "Pressupostos" e "Referências" a partir daqui, e a
# prosa da ajuda não as repete.
#
# As referências são as que já estavam conferidas na prosa das ajudas (autor,
# ano, título, edição, DOI copiados dela); a de implementação aponta a função
# que o bloco REALMENTE chama.

.tr_exp_P <- function(texto, verificar = NULL, se_falhar = NULL) {
  trama::tr_pressuposto(texto, verificar = verificar, se_falhar = se_falhar)
}

.tr_exp_impl <- function(pacote, funcao, nota = NULL) {
  trama::tr_ref(papel = "implementacao", pacote = pacote, funcao = funcao, nota = nota)
}

#' As referências usadas em mais de um bloco.
#' @noRd
.tr_exp_refs <- function() {
  R <- trama::tr_ref
  list(
    montgomery = function(nota = NULL) R(autores = "Montgomery, D. C.", ano = 2017,
      titulo = "Design and Analysis of Experiments", fonte = "9. ed. Hoboken: Wiley",
      papel = "livro-texto", nota = nota),
    gelman = function(nota = NULL) R(autores = c("Gelman, A.", "Hill, J."), ano = 2007,
      titulo = "Data Analysis Using Regression and Multilevel/Hierarchical Models",
      fonte = "Cambridge: Cambridge University Press", papel = "livro-texto", nota = nota),
    box_wilson = R(autores = c("Box, G. E. P.", "Wilson, K. B."), ano = 1951,
      titulo = "On the experimental attainment of optimum conditions",
      fonte = "Journal of the Royal Statistical Society, Series B, 13(1), 1–38 (com a discussão, até 45)",
      doi = "10.1111/j.2517-6161.1951.tb00067.x", papel = "teoria"),
    pimentel = R(autores = "Pimentel-Gomes, F.", ano = 2009,
      titulo = "Curso de Estatística Experimental", fonte = "15. ed. Piracicaba: FEALQ",
      papel = "livro-texto"),
    searle = R(autores = "Searle, S. R.", ano = 1971, titulo = "Linear Models",
      fonte = "New York: Wiley", papel = "livro-texto")
  )
}

#' Pressupostos que valem para todo nó estocástico de simulação.
#' @noRd
.tr_exp_docs <- function() {
  P <- .tr_exp_P; I <- .tr_exp_impl; R <- trama::tr_ref
  L <- .tr_exp_refs()
  sorteio_card <- "A semente é do card (não da sessão): o mesmo documento sorteia sempre os mesmos valores."
  anova_tab <- I("trama.models", "tr_models_anova_table",
    "O p de cada réplica é o do termo no quadro de ANOVA (SQ tipo I) do modelo ajustado pela análise escolhida (`stats::aov`, `lme4`/`lmerTest` na parcela subdividida).")
  list(
    "experiments/design" = list(
      pressupostos = list(
        P("O que se declara é o experimento que **será conduzido**: cada unidade recebe o tratamento sorteado, e o sorteio respeita a restrição do delineamento (dentro do bloco, da parcela, da linha e da coluna).",
          verificar = "experiments/view",
          se_falhar = "Se a alocação de campo não foi a sorteada, a análise perde a justificativa pela aleatorização; confira o mapa e a hierarquia no `experiments/view`."),
        P("Os blocos (e linhas, colunas, locais) agrupam unidades **parecidas**: a variação que se quer tirar do erro está entre blocos, não dentro deles.",
          se_falhar = "Sem fonte de variação conhecida, use o DIC; bloco que não separa nada só gasta gl do erro.")),
      referencias = list(
        R(autores = c("Banzatto, D. A.", "Kronka, S. N."), ano = 2006, titulo = "Experimentação agrícola",
          fonte = "4. ed. Jaboticabal: FUNEP", papel = "livro-texto"),
        L$montgomery(),
        R(autores = c("Cochran, W. G.", "Cox, G. M."), ano = 1957, titulo = "Experimental Designs",
          fonte = "2. ed. New York: Wiley", papel = "livro-texto"),
        R(autores = c("Box, G. E. P.", "Hunter, J. S.", "Hunter, W. G."), ano = 2005,
          titulo = "Statistics for Experimenters", fonte = "2. ed. Hoboken: Wiley", papel = "livro-texto"),
        L$box_wilson,
        R(autores = "Williams, E. J.", ano = 1949,
          titulo = "Experimental designs balanced for the estimation of residual effects of treatments",
          fonte = "Australian Journal of Scientific Research A, 2(2), 149–168",
          doi = "10.1071/CH9490149", papel = "teoria"),
        R(autores = c("Jones, B.", "Kenward, M. G."), ano = 2014, titulo = "Design and Analysis of Cross-Over Trials",
          fonte = "3. ed. Boca Raton: CRC Press", papel = "livro-texto"),
        R(autores = c("Fisher, R. A.", "Yates, F."), ano = 1963,
          titulo = "Statistical Tables for Biological, Agricultural and Medical Research",
          fonte = "6. ed. Edinburgh: Oliver and Boyd", papel = "livro-texto",
          nota = "As tabelas de quadrados latinos."),
        R(autores = c("Jacobson, M. T.", "Matthews, P."), ano = 1996,
          titulo = "Generating uniformly distributed random Latin squares",
          fonte = "Journal of Combinatorial Designs, 4(6), 405–437",
          doi = "10.1002/(SICI)1520-6610(1996)4:6<405::AID-JCD3>3.0.CO;2-J", papel = "teoria"),
        R(autores = "Bose, R. C.", ano = 1939, titulo = "On the construction of balanced incomplete block designs",
          fonte = "Annals of Eugenics, 9(4), 353–399", doi = "10.1111/j.1469-1809.1939.tb02219.x",
          papel = "teoria"),
        R(autores = "Bailey, R. A.", ano = 2008, titulo = "Design of Comparative Experiments",
          fonte = "Cambridge: Cambridge University Press", papel = "livro-texto"),
        I("base", "sample.int", paste(
          "O sorteio da alocação (permutações dentro de cada escopo do delineamento), com a semente do card.",
          "O número de quadrados latinos reduzidos de ordem n confere com a OEIS A000315 (https://oeis.org/A000315)."))) ),

    "experiments/view" = list(
      pressupostos = list(),
      referencias = list(
        R(autores = "Bailey, R. A.", ano = 2008, titulo = "Design of Comparative Experiments",
          fonte = "Cambridge: Cambridge University Press", papel = "livro-texto",
          nota = "Estrutura de unidades e de tratamentos, que as abas mapa e hierarquia desenham."),
        I("ggplot2", "ggplot"))),

    "experiments/effect" = list(
      pressupostos = list(
        P("Nenhum sobre dados: é simulação. O que se declara é o **modelo verdadeiro**, e o resultado de tudo o que vem depois vale para ele."),
        P("O efeito **aleatório** é normal de média zero com o desvio-padrão declarado; o **fixo** é de soma zero quando declarado por contraste (e o que se digitar, quando por nível).",
          verificar = "experiments/view"),
        P(sorteio_card)),
      referencias = list(
        L$montgomery("Contrastes e contrastes ortogonais, cap. 3."),
        L$gelman("Simulação de dados falsos para conferir a análise, cap. 8."),
        I("stats", "rnorm", "Os níveis do efeito aleatório e a covariável gerada; os contrastes por `stats::poly` (polinomiais)."))),

    "experiments/error" = list(
      pressupostos = list(
        P("Nenhum sobre dados: é simulação. O resíduo é **independente entre unidades**, salvo a correlação declarada dentro do indivíduo (simetria composta ou AR(1))."),
        P("A perda de parcelas é **completamente ao acaso** (MCAR), que é o caso em que a análise dos dados restantes não tem viés.",
          se_falhar = "Perda que depende da resposta ou do tratamento não se simula aqui; o poder e o tipo I obtidos valem só para MCAR."),
        P(sorteio_card)),
      referencias = list(
        R(autores = c("McCullagh, P.", "Nelder, J. A."), ano = 1989, titulo = "Generalized Linear Models",
          fonte = "2. ed. London: Chapman & Hall", papel = "livro-texto", nota = "Famílias e funções de ligação."),
        R(autores = c("Littell, R. C.", "Milliken, G. A.", "Stroup, W. W.", "Wolfinger, R. D.", "Schabenberger, O."),
          ano = 2006, titulo = "SAS for Mixed Models", fonte = "2. ed. Cary: SAS Institute",
          papel = "livro-texto", nota = "Simetria composta e AR(1) em medidas repetidas."),
        R(autores = c("Johnson, N. L.", "Kotz, S.", "Balakrishnan, N."), ano = 1994,
          titulo = "Continuous Univariate Distributions, v. 1", fonte = "2. ed. New York: Wiley",
          papel = "livro-texto", nota = "Assimetria da gama, 2/√k."),
        I("stats", "rnorm", "O resíduo normal; `stats::rpois`, `stats::rbinom` e `stats::rgamma` nas outras famílias, `stats::rt` nas caudas pesadas."))),

    "experiments/power" = list(
      pressupostos = list(
        P("O **modelo declarado** na cadeia (`experiments/effect` → `experiments/error`) é o que gera cada resposta: o poder vale para ele. Poder com σ declarado maior ou menor que o real é poder de outro experimento.",
          verificar = "experiments/view"),
        P("A **análise** escolhida é a do delineamento: com H0 verdadeira no modelo declarado, a taxa de rejeição tem de cair perto de α.",
          verificar = "experiments/power",
          se_falhar = "Se o tipo I foge de α (o `p_binomial` pequeno), a análise usa o erro errado — por exemplo, a parcela subdividida analisada como fatorial."),
        P("As réplicas são independentes: o erro de Monte Carlo da taxa é √(p(1 − p)/R), e o IC de Clopper-Pearson o mostra.",
          se_falhar = "IC largo demais: aumente as **Réplicas**.")),
      referencias = list(
        R(autores = c("Oliveira, I. R. C.", "Ferreira, D. F."), ano = 2010,
          titulo = "Multivariate extension of chi-squared univariate normality test",
          fonte = "Journal of Statistical Computation and Simulation, 80(5), 513–526",
          doi = "10.1080/00949650902731377", papel = "teoria",
          nota = "O critério do teste binomial exato para o tipo I em simulação."),
        R(autores = c("Clopper, C. J.", "Pearson, E. S."), ano = 1934,
          titulo = "The use of confidence or fiducial limits illustrated in the case of the binomial",
          fonte = "Biometrika, 26(4), 404–413", doi = "10.1093/biomet/26.4.404", papel = "teoria"),
        L$montgomery("Poder pelo F não central, cap. 3 — o oráculo dos testes."),
        L$gelman("Poder por simulação de dados falsos, cap. 20."),
        I("stats", "binom.test", "O IC exato de Clopper-Pearson da taxa e o `p_binomial` (taxa = α)."),
        anova_tab)),

    "experiments/randomization_test" = list(
      pressupostos = list(
        P("A alocação observada **saiu do sorteio** descrito no plano: o conjunto de re-sorteios é o das alocações admissíveis daquele delineamento.",
          verificar = "experiments/view",
          se_falhar = "Sem sorteio (ou com outro sorteio), o teste não tem a justificativa de Fisher; use a análise paramétrica e diga isso."),
        P("Sob H0, a resposta de cada unidade **não depende do tratamento** que ela recebeu (a nula exata: efeito zero em toda unidade, mais forte que a igualdade de médias). Não pede normalidade nem variâncias iguais.")),
      referencias = list(
        R(autores = "Fisher, R. A.", ano = 1935, titulo = "The Design of Experiments",
          fonte = "Edinburgh: Oliver and Boyd", papel = "teoria",
          nota = "Cap. III: os dados de Darwin em Zea mays."),
        R(autores = "Pitman, E. J. G.", ano = 1937,
          titulo = "Significance tests which may be applied to samples from any populations",
          fonte = "Supplement to the Journal of the Royal Statistical Society, 4(1), 119–130",
          doi = "10.2307/2984124", papel = "teoria"),
        R(autores = "Pitman, E. J. G.", ano = 1938,
          titulo = "Significance tests which may be applied to samples from any populations. III. The analysis of variance test",
          fonte = "Biometrika, 29(3/4), 322–335", doi = "10.2307/2332008", papel = "teoria"),
        R(autores = c("Edgington, E. S.", "Onghena, P."), ano = 2007, titulo = "Randomization Tests",
          fonte = "4. ed. Boca Raton: Chapman & Hall/CRC", papel = "livro-texto"),
        R(autores = c("Hinkelmann, K.", "Kempthorne, O."), ano = 2008,
          titulo = "Design and Analysis of Experiments, v. 1", fonte = "2. ed. Hoboken: Wiley",
          papel = "livro-texto", nota = "Aleatorização e análise pela aleatorização."),
        R(autores = c("Phipson, B.", "Smyth, G. K."), ano = 2010,
          titulo = "Permutation p-values should never be zero",
          fonte = "Statistical Applications in Genetics and Molecular Biology, 9(1)",
          doi = "10.2202/1544-6115.1585", papel = "teoria"),
        R(autores = c("Hothorn, T.", "Hornik, K.", "van de Wiel, M. A.", "Zeileis, A."), ano = 2008,
          titulo = "Implementing a class of permutation tests: the coin package",
          fonte = "Journal of Statistical Software, 28(8)", doi = "10.18637/jss.v028.i08",
          papel = "teoria", nota = "O `coin` é o oráculo dos testes."),
        I("trama.experiments", "tr_experiments_randomize",
          "Re-sorteia a alocação pela mesma receita do `experiments/design`; a estatística é o F do termo em `trama.models::tr_models_anova_table`."))),

    "experiments/contrasts" = list(
      pressupostos = list(
        P("Modelo **linear** com erro normal, independente e de variância constante (ANOVA, `lm`, misto ou parcela subdividida). GLM é recusado: lá o contraste vive na escala da ligação e não tem SQ que some.",
          verificar = c("models/shapiro_residuals", "models/levene", "models/plot_diagnostics"),
          se_falhar = "Para GLM, use o `models/linear_hypothesis`; com erro não normal, veja o `experiments/boxcox`."),
        P("Contrastes **planejados** antes de ver os dados: o p de cada linha não é corrigido para multiplicidade.",
          se_falhar = "Contrastes escolhidos depois de olhar as médias pedem Scheffé ou outra correção."),
        P("No desbalanceado, as médias são as ajustadas do `emmeans` e os SQ extras não somam o SQ do tratamento (o rodapé diz isso; o SQ de livro vai para `sq_livro`)."),
        P("Polinômios exigem fator **quantitativo**: os níveis têm de ser números, ou os valores vão em **Doses**.")),
      referencias = list(
        R(autores = "Dunnett, C. W.", ano = 1955,
          titulo = "A multiple comparison procedure for comparing several treatments with a control",
          fonte = "Journal of the American Statistical Association, 50(272), 1096–1121",
          doi = "10.1080/01621459.1955.10501294", papel = "teoria"),
        L$montgomery(paste("Cap. 3 (contrastes e contrastes ortogonais; o exemplo 3.1, da taxa de gravação,",
                           "reproduzido nos testes) e cap. 6 (o fatorial 2^k como contrastes; o 2² da seção 6.2).")),
        L$pimentel, L$searle,
        I("emmeans", "contrast", paste(
          "Estimativas e erros padrão sobre as médias do `emmeans::emmeans`, com `adjust = \"none\"`.",
          "O `sq` de cada linha é o SQ extra do teste de 1 gl, o mesmo do `car::linearHypothesis`.")),
        I("stats", "poly", "Os coeficientes polinomiais sobre as observações (níveis desigualmente espaçados e réplicas desiguais)."))),

    "experiments/boxcox" = list(
      pressupostos = list(
        P("Resposta **estritamente positiva**: y^λ não é definido para y ≤ 0.",
          se_falhar = "Se houver zeros, some uma constante antes (`data/mutate`) e registre isso."),
        P("Existe uma potência que normaliza e estabiliza a variância ao mesmo tempo: o λ é escolhido pela verossimilhança normal.",
          verificar = c("models/shapiro_residuals", "models/levene"),
          se_falhar = "Confira os resíduos do modelo transformado depois; se nenhuma potência serve, use um `models/glm`."),
        P("A transformação muda a escala da interpretação: as médias transformadas de volta são medianas, não médias, na escala original.")),
      referencias = list(
        R(autores = c("Box, G. E. P.", "Cox, D. R."), ano = 1964, titulo = "An analysis of transformations",
          fonte = "Journal of the Royal Statistical Society, Series B, 26(2), 211–243 (com a discussão, até 252)",
          doi = "10.1111/j.2517-6161.1964.tb00553.x", papel = "teoria",
          nota = "Os dados de venenos e tratamentos (`boot::poisons`) são deste artigo e estão nos testes."),
        R(autores = c("Venables, W. N.", "Ripley, B. D."), ano = 2002, titulo = "Modern Applied Statistics with S",
          fonte = "4. ed. New York: Springer", papel = "livro-texto", nota = "`MASS::boxcox`."),
        I("MASS", "boxcox", paste(
          "A conta (log-verossimilhança perfilada com a resposta dividida pela média geométrica) é a do `MASS::boxcox`,",
          "reescrita no bloco com `qr` sobre a matriz do modelo e `stats::optimize` para o ótimo exato; o MASS é o oráculo dos testes.")))),

    "experiments/response_surface" = list(
      pressupostos = list(
        P("Fatores já **codificados** (−1, 0, +1, ±α); a análise canônica só tem leitura nessa escala."),
        P("Erro normal, independente e de variância constante, como em todo `lm`.",
          verificar = c("models/shapiro_residuals", "models/plot_diagnostics")),
        P("O modelo de 2ª ordem é uma aproximação **local**: vale dentro da região experimentada, e o ponto estacionário fora dela não é recomendação."),
        P("A falta de ajuste só se testa com **pontos repetidos** (o erro puro vem das repetições no mesmo ponto).",
          se_falhar = "Sem pontos centrais repetidos, o quadro não parte o resíduo; planeje-os no `experiments/design`.")),
      referencias = list(
        L$box_wilson,
        R(autores = c("Myers, R. H.", "Montgomery, D. C.", "Anderson-Cook, C. M."), ano = 2009,
          titulo = "Response Surface Methodology", fonte = "3. ed. Hoboken: Wiley", papel = "livro-texto",
          nota = "Os dados do processo químico em dois blocos (`rsm::ChemReact`) são da tabela 7.6 desta edição e estão nos testes."),
        R(autores = "Lenth, R. V.", ano = 2009, titulo = "Response-Surface Methods in R, Using rsm",
          fonte = "Journal of Statistical Software, 32(7)", doi = "10.18637/jss.v032.i07", papel = "teoria"),
        I("stats", "lm", "O modelo de 1ª ou 2ª ordem; a canônica por `base::eigen`; o erro puro é o resíduo de `y ~ bloco + ponto`, a conta do `rsm`.")))
  )
}

#' Pressupostos e referências de um nó (listas vazias se não houver).
#' @noRd
.tr_exp_doc <- function(id) {
  d <- .tr_exp_docs()[[id]]
  list(pressupostos = d$pressupostos %||% list(), referencias = d$referencias %||% list())
}
