# Pressupostos e referências: distância, agrupamento, Tocher e MANOVA.

.tr_multi_docs_agrupamento <- function() {
  P <- .tr_multi_P; I <- .tr_multi_impl; R <- trama::tr_ref
  L <- .tr_multi_livros()
  sokal <- R(autores = c("Sokal, R. R.", "Rohlf, F. J."), ano = 1962,
             titulo = "The comparison of dendrograms by objective methods",
             fonte = "Taxon, 11(2), 33-40", doi = "10.2307/1217208")
  cruz <- R(autores = c("Cruz, C. D.", "Regazzi, A. J.", "Carneiro, P. C. S."), ano = 2012,
            titulo = "Modelos biométricos aplicados ao melhoramento genético",
            fonte = "v. 1, 4. ed. Viçosa: Editora UFV (ISBN 978-85-7269-433-9)", papel = "livro-texto")
  mahalanobis <- R(autores = "Mahalanobis, P. C.", ano = 1936, titulo = "On the generalised distance in statistics",
                   fonte = "Proceedings of the National Institute of Sciences of India, 2; reimpresso em Sankhyā A, 80(Supl. 1), 1-7, 2018",
                   doi = "10.1007/s13171-019-00164-5")
  escala <- P("As variáveis estão numa **escala comparável**, ou a distância cuida disso: na euclidiana crua a variável de maior variância domina; a padronizada, a Mahalanobis e a Gower corrigem.",
              se_falhar = "Use a `euclidiana padronizada` (ou Padronizar ligado no agrupamento).")
  sem_faltante <- P("**Sem faltantes** nas variáveis (o bloco recusa).",
                    se_falhar = "Ligue um `data/drop_na` antes, ou impute fora.")
  unidade <- P("Cada linha é **um indivíduo** (um genótipo, um acesso): com repetições do experimento, resuma antes (média por genótipo).",
               verificar = "data/group_summarise")
  list(
    "multi/distance" = list(
      pressupostos = list(unidade, sem_faltante, escala,
        P("Na **Mahalanobis**, a covariância S é a das próprias linhas e é **inversível** (mais linhas que variáveis, sem variável combinação exata das outras). Com a tabela de médias, o D² é descritivo: o dos livros de melhoramento usa a covariância RESIDUAL do experimento.",
          verificar = "multi/correlation_matrix",
          se_falhar = "Tire a variável redundante; para o D² com a covariância residual, faça a conta fora com a matriz E da MANOVA."),
        P("As variáveis medem **caracteres diferentes**: na euclidiana, duas muito correlacionadas contam o mesmo caráter duas vezes.",
          verificar = "multi/correlation_matrix",
          se_falhar = "Use a Mahalanobis, que desconta a correlação.")),
      referencias = list(mahalanobis,
        R(autores = "Gower, J. C.", ano = 1971, titulo = "A general coefficient of similarity and some of its properties",
          fonte = "Biometrics, 27(4), 857-871", doi = "10.2307/2528823"),
        cruz, L$johnson, L$ferreira,
        I("stats", "dist", "Euclidiana crua e padronizada (`scale`); o D² de Mahalanobis como euclidiana ao quadrado em X R⁻¹ (S = R'R pela Cholesky), conferido par a par contra `stats::mahalanobis` (1e-8)."),
        I("cluster", "daisy", "`metric = \"gower\"`; conferido contra a fórmula de Gower (1971) num exemplo resolvido à mão."))),

    "multi/cluster" = list(
      pressupostos = list(unidade, escala,
        P("A **ligação** combina com a distância: o Ward supõe distância euclidiana (sobre Mahalanobis usa a raiz do D²; sobre Gower é aproximado); UPGMA, completo e simples aceitam qualquer dissimilaridade.",
          verificar = "multi/distance"),
        P("O dendrograma **representa bem** a matriz: correlação cofenética acima de ~0,7 (Sokal & Rohlf 1962). Abaixo disso, os grupos do corte dizem pouco sobre as distâncias reais.",
          verificar = "multi/plot_dendrogram",
          se_falhar = "Troque a ligação (o UPGMA costuma dar a maior cofenética) ou leia a matriz direto no mapa de calor do `multi/distance`."),
        P("O **número de grupos** é uma escolha, não uma estimativa: nenhum teste aqui diz que há k grupos. Todo agrupamento acha grupos, até em dados sem estrutura.",
          verificar = c("multi/plot_dendrogram", "multi/tocher"),
          se_falhar = "Corte onde os ramos são longos, e confira se o Tocher na mesma matriz dá grupos parecidos."),
        P("No **k-means**, os grupos são aproximadamente esféricos e de tamanhos parecidos, e o ótimo é LOCAL (por isso 25 partidas e a semente do nó).")),
      referencias = list(
        R(autores = c("Lance, G. N.", "Williams, W. T."), ano = 1967,
          titulo = "A general theory of classificatory sorting strategies: 1. Hierarchical systems",
          fonte = "The Computer Journal, 9(4), 373-380", doi = "10.1093/comjnl/9.4.373"),
        R(autores = "Ward, J. H.", ano = 1963, titulo = "Hierarchical grouping to optimize an objective function",
          fonte = "Journal of the American Statistical Association, 58(301), 236-244",
          doi = "10.1080/01621459.1963.10500845"),
        R(autores = c("Murtagh, F.", "Legendre, P."), ano = 2014,
          titulo = "Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion?",
          fonte = "Journal of Classification, 31(3), 274-295", doi = "10.1007/s00357-014-9161-z",
          papel = "complementar"),
        sokal,
        R(autores = c("Hartigan, J. A.", "Wong, M. A."), ano = 1979, titulo = "Algorithm AS 136: a k-means clustering algorithm",
          fonte = "Applied Statistics, 28(1), 100-108", doi = "10.2307/2346830"),
        cruz, L$johnson,
        I("stats", "hclust", "`average`, `ward.D2`, `complete`, `single`; cofenética por `stats::cophenetic`, corte por `stats::cutree`. Alturas de fusão conferidas contra `cluster::agnes` nas quatro ligações (1e-10)."),
        I("stats", "kmeans", "Hartigan-Wong, `nstart = 25`, `iter.max = 100`."))),

    "multi/tocher" = list(
      pressupostos = list(unidade,
        P("A matriz de distância é a **adequada** aos caracteres (em melhoramento, o D² de Mahalanobis ou a euclidiana padronizada): o Tocher só reorganiza a matriz que recebe.",
          verificar = "multi/distance"),
        P("O critério θ (a maior das menores distâncias) supõe que **a distância média dentro de um grupo fica abaixo da distância entre grupos**; indivíduos muito isolados viram grupos de um, e o último grupo pode juntar os que sobraram.",
          se_falhar = "Leia os grupos de um como indivíduos divergentes, e confira contra o dendrograma do `multi/cluster`."),
        P("O resultado **depende da ordem** de formação (cada grupo começa pelo par mais próximo que sobra): não há teste nem medida de ajuste.")),
      referencias = list(
        R(autores = "Rao, C. R.", ano = 1952, titulo = "Advanced Statistical Methods in Biometric Research",
          fonte = "New York: Wiley", papel = "livro-texto"),
        cruz,
        I("trama.multi", "tr_multi_tocher", "Implementação própria do algoritmo de Cruz, Regazzi & Carneiro (inclusão enquanto a distância média do candidato ao grupo não passa de θ); conferida num exemplo resolvido à mão e contra `biotools::tocher` no `garlicdist` (θ a 1e-6)."))),

    "multi/manova" = list(
      pressupostos = list(
        P("As observações (parcelas) são **independentes**.",
          se_falhar = "Medidas repetidas no tempo pedem um modelo de medidas repetidas, fora deste bloco."),
        P("Os resíduos são **normais multivariados**.",
          verificar = c("multi/mardia", "view/qq"),
          se_falhar = "Transforme as respostas assimétricas (log) e prefira a de Pillai; com tratamentos de mesmo número de repetições as quatro estatísticas sofrem menos."),
        P("A **matriz de covariância** é a mesma em todos os tratamentos.",
          verificar = "multi/box_m",
          se_falhar = "Use a de Pillai, a mais robusta a covariâncias diferentes (Olson 1976), e mantenha os tratamentos com o mesmo número de repetições; o M de Box rejeita com facilidade."),
        P("Há **mais repetições do que respostas**: o resíduo precisa de graus de liberdade (o bloco recusa senão), e com poucas o teste tem pouco poder."),
        P("Com **bloco**, o modelo é aditivo (sem interação bloco × tratamento), como no DBC.",
          verificar = "models/tukey_additivity"),
        P("O F de **Roy** é um limite superior: o p-valor dele sai otimista.")),
      referencias = list(
        R(autores = "Wilks, S. S.", ano = 1932, titulo = "Certain generalizations in the analysis of variance",
          fonte = "Biometrika, 24(3-4), 471-494", doi = "10.1093/biomet/24.3-4.471"),
        R(autores = "Pillai, K. C. S.", ano = 1955, titulo = "Some new test criteria in multivariate analysis",
          fonte = "The Annals of Mathematical Statistics, 26(1), 117-121", doi = "10.1214/aoms/1177728599"),
        R(autores = "Olson, C. L.", ano = 1976, titulo = "On choosing a test statistic in multivariate analysis of variance",
          fonte = "Psychological Bulletin, 83(4), 579-586", doi = "10.1037/0033-2909.83.4.579",
          papel = "complementar"),
        L$johnson, L$ferreira,
        I("stats", "summary.manova", "`manova(Y ~ bloco + tratamento)`, SQ sequencial; conferido contra `car::Manova` (H e E próprias, Pillai e Wilks com e sem bloco, a 1e-8)."))))
}
