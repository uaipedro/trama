# A coleção `multi`: o registro dos tipos, das categorias e dos nós.
#
# As declarações dos nós moram ao lado das funções (`.tr_multi_nos_pca()` em
# `R/pca.R`, e assim por diante), e não num arquivo único de duas mil linhas
# como na `series`: cada técnica tem a sua página de ajuda longa, e ler o `fn`
# com a página ao lado é o que pega a ajuda que promete o que o código não faz.

#' A coleção `multi`.
#'
#' Carrega DEPOIS de `trama.data`, `trama.view` e `trama.models`: as portas usam
#' `data/table`, `view/plot` e `models/fit`, e o registro recusa porta com tipo
#' desconhecido (o `Config/trama/requires` do DESCRIPTION diz isso ao núcleo).
#'
#' Os ids de categoria têm prefixo (`multi_*`) porque o registro de categorias
#' é GLOBAL: uma categoria `pca` aqui sobrescreveria em silêncio a de outra
#' coleção que usasse o mesmo id.
#' @export
trama_collection <- function() {
  trama::tr_collection(
    id = "multi", version = "0.3.0", label = "Multivariada",
    # A discriminante e a logística saem como `models/fit` (tipo da
    # `trama.models`, que por isso carrega antes): prever, confundir e a ROC
    # são os blocos de lá.
    transitions = trama::tr_transitions_read(system.file("trama/transicoes.json", package = "trama.multi")),
    types = list(multi_pca_type(), multi_fa_type(), multi_dist_type(), multi_cluster_type()),
    adapters = .tr_multi_adapters(),
    # Os gráficos ficam na categoria da técnica, e não numa "Ver" à parte: quem
    # fez uma PCA procura o biplot ao lado dela.
    categories = list(
      trama::tr_category("multi_fonte",         "Fonte", role = "origem"),
      trama::tr_category("multi_diagnostico",   "Diagnóstico", role = "inspecao"),
      trama::tr_category("multi_pca",           "Componentes", role = "ajuste"),
      trama::tr_category("multi_fatorial",      "Fatorial", role = "ajuste"),
      trama::tr_category("multi_discriminante", "Classificação", role = "ajuste"),
      trama::tr_category("multi_logistica",     "Logística", role = "ajuste"),
      trama::tr_category("multi_manova",        "MANOVA", role = "avaliacao"),
      # Distância, agrupamento, dendrograma e Tocher juntos: o estudo de
      # diversidade genética é essa sequência, e procura-se uma peça ao lado da outra.
      trama::tr_category("multi_agrupamento",   "Agrupamento", role = "ajuste"),
      trama::tr_category("multi_jackknife",     "Jackknife", role = "avaliacao")
    ),
    nodes = c(.tr_multi_nos_fonte(), .tr_multi_nos_diagnostico(), .tr_multi_nos_correlacao(),
              .tr_multi_nos_pca(),
              .tr_multi_nos_fatorial(), .tr_multi_nos_discriminante(),
              .tr_multi_nos_logistica(), .tr_multi_nos_jackknife(),
              .tr_multi_nos_manova(), .tr_multi_nos_agrupamento()),
    # Glossário de params (docs/glossario-parametros.md): fluxos salvos com os
    # nomes antigos abrem com os novos. Na discriminante e na logística o grupo
    # conhecido é a RESPOSTA e as medidas são os PREDITORES; no M de Box `grupo`
    # e `cols` continuam, porque lá não há o que prever. `nivel` era a confiança
    # do intervalo e já guardava 0,95: renome puro. Os blocos que foram para a
    # `trama.models` (classify, confusion, roc, logistic_coefficients) têm a
    # migração declarada LÁ: o destino é da models, e as chaves de params são o
    # id novo.
    migrations = list(params = c(
      list(
        "multi/discriminant" = list(grupo = list(to = "resposta"), cols = list(to = "preditores")),
        "multi/logistic" = list(grupo = list(to = "resposta"), cols = list(to = "preditores"))),
      stats::setNames(
        rep(list(list(nivel = list(to = "confianca"))), 4L),
        c("multi/jackknife_pca", "multi/jackknife_fa",
          "multi/jackknife_discriminant", "multi/jackknife_logistic"))))
  )
}
