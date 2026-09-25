# Pressupostos e referências: o jackknife das quatro técnicas.

.tr_multi_docs_jackknife <- function() {
  P <- .tr_multi_P; I <- .tr_multi_impl; R <- trama::tr_ref
  L <- .tr_multi_livros()
  quenouille <- R(autores = "Quenouille, M. H.", ano = 1956, titulo = "Notes on bias in estimation",
                  fonte = "Biometrika, 43(3-4), 353-360", doi = "10.1093/biomet/43.3-4.353")
  efron_stein <- R(autores = c("Efron, B.", "Stein, C."), ano = 1981, titulo = "The jackknife estimate of variance",
                   fonte = "The Annals of Statistics, 9(3), 586-596", doi = "10.1214/aos/1176345462")
  efron82 <- R(autores = "Efron, B.", ano = 1982,
               titulo = "The Jackknife, the Bootstrap and Other Resampling Plans",
               fonte = "Philadelphia: SIAM", doi = "10.1137/1.9781611970319", papel = "livro-texto")
  impl <- I("trama.multi", "tr_multi_jackknife",
            "Implementação própria: n reajustes sem uma linha; viés (n − 1)(média − θ), erro padrão √((n − 1)/n · Σ(θ₍ᵢ₎ − média)²) e intervalo corrigida ± t(n − 1) · EP.")
  indep <- P("As linhas são **independentes e identicamente distribuídas**: o jackknife tira uma linha de cada vez, e com dados agrupados ou em série a variância sai subestimada.",
             se_falhar = "Jackknife por grupo (tirar um bloco inteiro) ainda sem bloco no trama (lacuna registrada).")
  suave <- P("A estatística é uma função **suave** dos dados: o jackknife falha em estatísticas que saltam (quantis, e autovalores quase iguais que trocam de ordem entre réplicas), e o erro padrão sai inflado.",
             verificar = "data/arrange",
             se_falhar = "Ordene a tabela `pseudovalores` por `sem_ela` num `data/arrange` e procure dois patamares; se houver, não use o erro padrão daquela estatística.")
  normal <- P("O **intervalo** (corrigida ± t · EP) supõe a estatística aproximadamente normal; o erro padrão do jackknife tende a ser um pouco **conservador** (viesado para cima).")
  refs <- function(...) list(quenouille, efron_stein, efron82, L$efron_tib, ..., impl)
  list(
    "multi/jackknife_pca" = list(
      pressupostos = list(indep, suave, normal,
        P("A PCA de entrada tem a **escala** certa (padronizada ou não): as réplicas repetem a mesma escolha.",
          verificar = "multi/pca")),
      referencias = refs(L$johnson)),
    "multi/jackknife_fa" = list(
      pressupostos = list(indep, suave, normal,
        P("O modelo fatorial de entrada é estável: cada réplica precisa convergir sem caso Heywood, com o mesmo número de fatores (a réplica que falha vira erro que nomeia a linha).",
          verificar = c("multi/kmo_bartlett", "multi/parallel")),
        P("Os fatores das réplicas se **casam** com os da amostra toda pela congruência de Tucker; com fatores mal definidos (congruência baixa) o casamento é arbitrário.",
          verificar = "multi/plot_loadings")),
      referencias = refs(
        R(autores = c("Lorenzo-Seva, U.", "ten Berge, J. M. F."), ano = 2006,
          titulo = "Tucker's congruence coefficient as a meaningful index of factor similarity",
          fonte = "Methodology, 2(2), 57-64", doi = "10.1027/1614-2241.2.2.57", papel = "complementar"))),
    "multi/jackknife_discriminant" = list(
      pressupostos = list(indep, suave, normal,
        P("A discriminante de entrada é **linear** (o bloco recusa a quadrática) e atende aos seus pressupostos.",
          verificar = "multi/box_m"),
        P("Cada grupo continua com observações suficientes quando uma linha sai; grupos muito pequenos deixam as correlações canônicas instáveis.",
          verificar = "data/group_summarise")),
      referencias = refs(L$johnson)),
    "multi/jackknife_logistic" = list(
      pressupostos = list(indep, suave, normal,
        P("**Sem separação** no modelo e em nenhuma réplica: o bloco recusa o modelo separado, e uma réplica que separa vira erro que nomeia a linha.",
          se_falhar = "Tire o preditor que isola os grupos, ou use a `multi/discriminant`.")),
      referencias = refs(L$hosmer))
  )
}
