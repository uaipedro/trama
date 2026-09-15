# A coleção `models`: o registro dos tipos, das categorias e dos nós.
#
# As declarações dos nós moram em `R/nos_*.R`, por aba, e não num arquivo único
# como na `series`: são trinta e quatro páginas de ajuda, e ler a página com o
# grupo de funções ao lado é o que pega a ajuda que promete o que o código não
# faz.

#' A coleção `models`.
#'
#' Carrega DEPOIS de `trama.data` e `trama.view`: as portas usam `data/table` e
#' `view/plot`, e o registro recusa porta com tipo desconhecido.
#'
#' Os ids de categoria têm prefixo (`modelo_*`) porque o registro de categorias
#' é GLOBAL: uma categoria `testes` aqui sobrescreveria em silêncio a de outra
#' coleção que usasse o mesmo id.
#' @export
trama_collection <- function() {
  trama::tr_collection(
    id = "models", version = "0.1.0", label = "Modelos",
    js = "trama/index.js", css = "trama/models.css",
    types = list(models_fit_type(), models_effects_type(), models_test_type(), models_emm_type()),
    adapters = .tr_models_adapters(),
    # O corte das abas é pela PERGUNTA: ajustar, ler o ajuste, comparar médias,
    # conferir os pressupostos, testar sem modelo. Os delineamentos têm aba
    # própria porque quem planta um experimento procura "DBC", e não "lm".
    categories = list(
      trama::tr_category("modelo_fonte",        "Fonte",        "#10b981"),
      trama::tr_category("modelo_ajustar",      "Ajustar",      "#14b8a6"),
      trama::tr_category("modelo_anova",        "ANOVA",        "#0ea5e9"),
      trama::tr_category("modelo_resumir",      "Resumir",      "#eab308"),
      trama::tr_category("modelo_medias",       "Médias",       "#8b5cf6"),
      trama::tr_category("modelo_pressupostos", "Pressupostos", "#f97316"),
      trama::tr_category("modelo_testes",       "Testes",       "#ef4444")
    ),
    nodes = c(.tr_models_nos_fonte(), .tr_models_nos_ajustar(), .tr_models_nos_anova(),
              .tr_models_nos_resumir(), .tr_models_nos_medias(),
              .tr_models_nos_pressupostos(), .tr_models_nos_testes())
  )
}
