# A coleção `multi`: o registro dos tipos, das categorias e dos nós.
#
# As declarações dos nós moram ao lado das funções (`.tr_multi_nos_pca()` em
# `R/pca.R`, e assim por diante), e não num arquivo único de duas mil linhas
# como na `series`: cada técnica tem a sua página de ajuda longa, e ler o `fn`
# com a página ao lado é o que pega a ajuda que promete o que o código não faz.

#' A coleção `multi`.
#'
#' Carrega DEPOIS de `trama.data` e `trama.view`: as portas usam `data/table` e
#' `view/plot`, e o registro recusa porta com tipo desconhecido.
#'
#' Os ids de categoria têm prefixo (`multi_*`) porque o registro de categorias
#' é GLOBAL: uma categoria `pca` aqui sobrescreveria em silêncio a de outra
#' coleção que usasse o mesmo id.
#' @export
trama_collection <- function() {
  trama::tr_collection(
    id = "multi", version = "0.1.0", label = "Multivariada",
    transitions = trama::tr_transitions_read(system.file("trama/transicoes.json", package = "trama.multi")),
    types = list(multi_pca_type(), multi_fa_type(), multi_lda_type(), multi_logit_type(),
                 multi_classifier_type()),
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
      trama::tr_category("multi_jackknife",     "Jackknife", role = "avaliacao")
    ),
    nodes = c(.tr_multi_nos_fonte(), .tr_multi_nos_diagnostico(), .tr_multi_nos_correlacao(),
              .tr_multi_nos_pca(),
              .tr_multi_nos_fatorial(), .tr_multi_nos_discriminante(),
              .tr_multi_nos_logistica(), .tr_multi_nos_roc(), .tr_multi_nos_pr(), .tr_multi_nos_jackknife())
  )
}
