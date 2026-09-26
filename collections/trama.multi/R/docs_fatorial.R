# Pressupostos e referências: PCA, análise fatorial e a adequação da matriz.

.tr_multi_docs_fatorial <- function() {
  P <- .tr_multi_P; I <- .tr_multi_impl; R <- trama::tr_ref
  L <- .tr_multi_livros()
  kaiser70 <- R(autores = "Kaiser, H. F.", ano = 1970, titulo = "A second generation little jiffy",
                fonte = "Psychometrika, 35(4), 401-415", doi = "10.1007/BF02291817")
  kaiser74 <- R(autores = "Kaiser, H. F.", ano = 1974, titulo = "An index of factorial simplicity",
                fonte = "Psychometrika, 39(1), 31-36", doi = "10.1007/BF02291575")
  bartlett50 <- R(autores = "Bartlett, M. S.", ano = 1950, titulo = "Tests of significance in factor analysis",
                  fonte = "British Journal of Statistical Psychology, 3(2), 77-85",
                  doi = "10.1111/j.2044-8317.1950.tb00285.x")
  linear <- P("As variáveis se relacionam de forma **linear**: PCA e fatorial trabalham com a matriz de correlação (ou covariância), que só enxerga associação linear.",
              verificar = c("multi/plot_correlation", "view/points"),
              se_falhar = "Transforme a variável curva (log, raiz) num `data/mutate` antes.")
  sem_extremos <- P("Sem **observações extremas** que dominem as correlações: uma linha fora da nuvem pode criar ou apagar um componente/fator sozinha.",
                    verificar = c("view/points", "view/boxplot"),
                    se_falhar = "Refaça sem a linha (`data/filter`) e compare; o jackknife da técnica mede a influência de cada linha.")
  list(
    "multi/pca" = list(
      pressupostos = list(
        P("A PCA **não supõe modelo nem distribuição**: é uma rotação dos dados. O que ela exige é a escolha da **escala** — com variáveis em unidades diferentes, sem padronizar, a de maior variância domina o CP1.",
          se_falhar = "Ligue **Padronizar** (matriz de correlação), que é o caso comum."),
        linear, sem_extremos,
        P("As variáveis são **correlacionadas**: se não forem, cada componente é praticamente uma variável e a redução não resume nada.",
          verificar = c("multi/kmo_bartlett", "multi/correlation_matrix"))),
      referencias = list(
        R(autores = "Pearson, K.", ano = 1901, titulo = "On lines and planes of closest fit to systems of points in space",
          fonte = "The London, Edinburgh, and Dublin Philosophical Magazine and Journal of Science, 2(11), 559-572",
          doi = "10.1080/14786440109462720"),
        R(autores = "Hotelling, H.", ano = 1933,
          titulo = "Analysis of a complex of statistical variables into principal components",
          fonte = "Journal of Educational Psychology, 24(6), 417-441", doi = "10.1037/h0071325"),
        R(autores = c("Jolliffe, I. T.", "Cadima, J."), ano = 2016,
          titulo = "Principal component analysis: a review and recent developments",
          fonte = "Philosophical Transactions of the Royal Society A, 374(2065), 20150202",
          doi = "10.1098/rsta.2015.0202", papel = "complementar"),
        L$johnson, L$mingoti, L$ferreira,
        I("stats", "prcomp", "Decomposição em valores singulares da matriz centrada (`center = TRUE` sempre); `scale. = padronizar`."))),

    "multi/factor_analysis" = list(
      pressupostos = list(
        P("Vale o **modelo de fatores comuns**: cada variável é combinação linear de poucos fatores latentes mais um erro próprio (a unicidade), com erros não correlacionados entre si e com os fatores.",
          verificar = "multi/fa_loadings",
          se_falhar = "Se o objetivo é só resumir a variância, sem construto latente, use a `multi/pca`."),
        P("A matriz tem **correlação suficiente** para fatorar: KMO global de pelo menos 0,6 e esfericidade de Bartlett rejeitada.",
          verificar = "multi/kmo_bartlett",
          se_falhar = "Retire as variáveis de MSA baixo (menor que 0,5) e refaça."),
        P("O **número de fatores** é o certo e o modelo se identifica (gl ≥ 0; o bloco recusa senão). Fatores demais levam a casos Heywood.",
          verificar = "multi/parallel",
          se_falhar = "Compare também o `multi/scree` de uma `multi/pca` padronizada nas mesmas variáveis, e ajuste com um fator a menos."),
        P("No método **ml**, as variáveis são **normais multivariadas**: o teste qui-quadrado de ajuste depende disso. O `paf` não supõe distribuição.",
          verificar = c("multi/mardia", "models/shapiro"),
          se_falhar = "Com o `multi/mardia` rejeitando, o teste χ² e os erros da ML não valem como publicados. Com itens Likert ou dados assimétricos, use `metodo = \"paf\"`."),
        linear,
        P("Amostra **grande** em relação ao número de variáveis: cargas e comunalidades são instáveis com poucas observações.",
          verificar = "multi/jackknife_fa")),
      referencias = list(
        R(autores = "Lawley, D. N.", ano = 1940,
          titulo = "The estimation of factor loadings by the method of maximum likelihood",
          fonte = "Proceedings of the Royal Society of Edinburgh, 60(1), 64-82", doi = "10.1017/S037016460002006X"),
        R(autores = "Kaiser, H. F.", ano = 1958, titulo = "The varimax criterion for analytic rotation in factor analysis",
          fonte = "Psychometrika, 23(3), 187-200", doi = "10.1007/BF02289233", papel = "complementar"),
        R(autores = c("Hendrickson, A. E.", "White, P. O."), ano = 1964,
          titulo = "Promax: a quick method for rotation to oblique simple structure",
          fonte = "British Journal of Statistical Psychology, 17(1), 65-70",
          doi = "10.1111/j.2044-8317.1964.tb00244.x", papel = "complementar"),
        L$johnson, L$mingoti, L$ferreira,
        I("stats", "factanal", "Método `ml`: `factanal(covmat = R, n.obs = n, rotation = \"none\")`, unicidades limitadas em 0,005. Método `paf`, as rotações (varimax, quartimax, equamax, promax, oblimin) e os escores de regressão e de Bartlett são implementação própria da coleção."))),

    "multi/kmo_bartlett" = list(
      pressupostos = list(
        P("As observações são **independentes**, e o teste de Bartlett supõe **normalidade multivariada**; com n grande ele rejeita quase sempre, e por isso só diz se há ALGUMA correlação.",
          verificar = c("multi/mardia", "models/shapiro"),
          se_falhar = "Com o `multi/mardia` rejeitando, o p do Bartlett não é confiável; leia o KMO, que não depende de distribuição."),
        P("A matriz de correlação é **inversível** (nenhuma variável é combinação exata das outras): o KMO usa as correlações parciais, que vêm da inversa.",
          verificar = "multi/correlation_matrix",
          se_falhar = "Tire a variável redundante da lista.")),
      referencias = list(kaiser70, kaiser74, bartlett50,
        R(autores = "Bartlett, M. S.", ano = 1951, titulo = "A further note on tests of significance in factor analysis",
          fonte = "British Journal of Statistical Psychology, 4(1), 1-2",
          doi = "10.1111/j.2044-8317.1951.tb00299.x", papel = "complementar"),
        L$mingoti, L$ferreira,
        I("trama.multi", "tr_multi_kmo_bartlett", "Implementação própria: KMO e MSA pelas correlações parciais da inversa de R; χ² = −(n − 1 − (2p + 5)/6) ln|R| com p(p − 1)/2 gl.")))
  )
}
