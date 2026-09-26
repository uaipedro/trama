# A coleção `sampling`: o registro dos tipos, das categorias e dos nós.
#
# As declarações dos nós moram em `R/nos_*.R`, por aba, como na `models`: ler a
# página de ajuda com o grupo de funções ao lado é o que pega a ajuda que
# promete o que o código não faz.

#' A coleção `sampling`.
#'
#' Carrega DEPOIS de `trama.data` e `trama.view`: as portas usam `data/table` e
#' `view/plot`, e o registro recusa porta com tipo desconhecido.
#'
#' Os ids de categoria têm prefixo (`amostra_*`) porque o registro de categorias
#' é GLOBAL: uma categoria `estimar` aqui sobrescreveria em silêncio a de outra
#' coleção que usasse o mesmo id.
#' @export
trama_collection <- function() {
  nos <- c(.tr_sampling_nos_fonte(), .tr_sampling_nos_planejar(), .tr_sampling_nos_domains(), .tr_sampling_nos_precisao(), .tr_sampling_nos_perguntas(),
           .tr_sampling_nos_selecionar(), .tr_sampling_nos_desenho(), .tr_sampling_nos_rake(), .tr_sampling_nos_estimar(), .tr_sampling_nos_avaliar())
  # A confiança era string ("95%") e virou número: converte no lugar todo nó
  # que tem o param. `when = is.character` deixa intacto o fluxo já migrado.
  conf <- list(confianca = list(to = "confianca", when = is.character,
                                value = function(v) as.numeric(sub("%", "", v, fixed = TRUE)) / 100))
  com_conf <- Filter(function(nd) "confianca" %in% names(nd$params), nos)
  params <- stats::setNames(rep(list(conf), length(com_conf)), vapply(com_conf, function(nd) nd$id, ""))
  params[["sampling/size_mean"]] <- c(params[["sampling/size_mean"]], list(coluna = list(to = "variavel")))
  trama::tr_collection(
    id = "sampling", version = "0.3.0", label = "Amostragem",
    transitions = trama::tr_transitions_read(system.file("trama/transicoes.json", package = "trama.sampling")),
    js = "trama/index.js", css = "trama/sampling.css",
    types = list(sampling_plan_type(), sampling_sample_type(), sampling_estimate_type(),
                 sampling_simulation_type()),
    adapters = .tr_sampling_adapters(),
    # O corte das abas segue o CAMINHO de uma pesquisa: planejar o tamanho (ou,
    # com o campo já dado, medir a precisão que ele alcança),
    # sortear, declarar ou calibrar o desenho, estimar, e avaliar o desenho
    # antes de ir a campo.
    categories = list(
      trama::tr_category("amostra_fonte",      "Fonte", role = "origem"),
      trama::tr_category("amostra_planejar",   "Planejar", role = "preparacao"),
      trama::tr_category("amostra_selecionar", "Selecionar", role = "preparacao"),
      trama::tr_category("amostra_desenho",    "Desenho", role = "preparacao"),
      trama::tr_category("amostra_precisao",   "Precisão", role = "avaliacao"),
      trama::tr_category("amostra_estimar",    "Estimar", role = "ajuste"),
      trama::tr_category("amostra_avaliar",    "Avaliar", role = "avaliacao")
    ),
    nodes = nos,
    # Glossário de params (docs/glossario-parametros.md): fluxos salvos com o
    # nome antigo abrem já migrados.
    migrations = list(params = params)
  )
}
